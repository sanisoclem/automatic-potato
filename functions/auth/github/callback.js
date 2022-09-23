import { v4 } from 'uuid';
import { sign } from 'jsonwebtoken';

export const onRequestGet = async ({ env, request }) => {
  const clientId = env.AUTH_GITHUB_CLIENT_ID;
  const clientSecret = env.AUTH_GITHUB_CLIENT_SECRET;
  const callbackUri = env.AUTH_GITHUB_CALLBACK_URI;
  const url = new URL(request.url);

  if (!url.searchParams.has('state') || !url.searchParams.has('code')) {
    return new Response("Invalid auth callback", { status: 400 });
  }
  const state = url.searchParams.get('state');
  const code = url.searchParams.get('code');

  const savedState = await env.EB.get(`auth:github:${state}`);

  if (state !== savedState) {
    return new Response("Invalid auth callback", { status: 400 });
  }
jsonwebtoken
  const tokenResp = await fetch('https://github.com/login/oauth/access_token?' + new URLSearchParams({
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
  const tokenData = await tokenResp.json();

  const userResp = await fetch('https://api.github.com/user', {
    headers: new Headers({
      'Accept': 'application/json',
      'Authorization': `${tokenData.token_type} ${tokenData.access_token}`,
      'User-Agent': 'Empire Builder'
    })
  });
  const userData = await userResp.json();

  const userDoc = {
    userId: v4().replace('-',''),
    name: userData.name,
    providers: {
      github: {
        token: tokenData.access_token,
        profile: userData
      }
    }
  }

  await env.EB.put(`user:${userDoc.userId}`, JSON.stringify(userDoc));

  // create jwt?

  return new Response(JSON.stringify({
    user,
    data
  }));
}
