import * as jose from 'jose'

const getAccessToken = async ({ clientId, clientSecret, callbackUri, code }) => {
  const resp = await fetch('https://github.com/login/oauth/access_token?' + new URLSearchParams({
    client_id: clientId,
    client_secret: clientSecret,
    code,
    redirect_uri: callbackUri
  }), {
    method: 'POST',
    headers: new Headers({
      'Accept': 'application/json'
    })
  });
  return await resp.json();
}

const getUserInfo = async({ tokenType, token }) => {
  const userResp = await fetch('https://api.github.com/user', {
    headers: new Headers({
      'Accept': 'application/json',
      'Authorization': `${tokenType} ${token}`,
      'User-Agent': 'Empire Builder'
    })
  });
  return await userResp.json();
}

export const onRequestGet = async ({ env, request }) => {
  const clientId = env.AUTH_GITHUB_CLIENT_ID;
  const clientSecret = env.AUTH_GITHUB_CLIENT_SECRET;
  const callbackUri = env.AUTH_GITHUB_CALLBACK_URI;
  const signingKey = env.AUTH_JWT_PRIVATE_KEY;
  const url = new URL(request.url);

  try
  {
    if (!url.searchParams.has('state') || !url.searchParams.has('code')) {
      return new Response("Invalid auth callback", { status: 400 });
    }
    const state = url.searchParams.get('state');
    const code = url.searchParams.get('code');
    const savedState = await env.EB.get(`auth:github:${state}`, { type: 'json' });

    if (savedState === null) {
      return new Response("Invalid auth callback", { status: 400 });
    }

    const tokenData = await getAccessToken({ clientId, clientSecret, callbackUri, code });
    const userData = await getUserInfo({ token: tokenData.access_token, tokenType: tokenData.token_type })

    // TODO: create an auth provider agnostic user
    // this is good for now, since KV isn't a good match for this (and I'll probably rewrite this in purs)
    // a single durable object could work
    // or an actual db (surreal?!?!?!)
    const userDoc = {
      userId: userData.id,
      name: userData.name,
      providers: {
        github: {
          token: tokenData.access_token,
          profile: userData
        }
      }
    }

    await env.EB.put(`user:${userDoc.userId}`, JSON.stringify(userDoc));

    const token = await new jose.SignJWT({ 'allow': userData.id === 758633, 'name': userData.name })
    .setSubject(userDoc.userId)
    .setIssuer(env.AUTH_JWT_ISSUER)
    .setAudience(env.AUTH_JWT_AUDIENCE)
    .setProtectedHeader({ alg: 'ES384' })
    .setExpirationTime('2h')
    .sign(await jose.importPKCS8(signingKey, 'ES384'))

    const resp = Response.redirect(`${url.origin}${savedState.returnUrl}`);
    resp.headers.set('Set-Cookie', `authToken=${token}; path=/api; secure; HttpOnly; SameSite=Strict`)
    return resp;
  }
  catch (err) {
    return new Response(JSON.stringify(err));
  }
}
