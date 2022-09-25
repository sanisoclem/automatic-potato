import { v4 } from 'uuid';

export const onRequestGet = async ({ env, request }) => {
  const clientId = env.AUTH_GITHUB_CLIENT_ID;
  const callbackUri = env.AUTH_GITHUB_CALLBACK_URI;
  const url = new URL(request.url);
  const state = v4();

  await env.EB.put(
    `auth:github:${state}`,
    JSON.stringify({
      returnUrl: url.searchParams.get('returnUrl') || '/'
    }),
    {
      expirationTtl: 120
    }
  );

  return Response.redirect(`https://github.com/login/oauth/authorize?client_id=${encodeURIComponent(clientId)}&redirect_uri=${encodeURIComponent(callbackUri)}&scope=user%3Aemail&state=${encodeURIComponent(state)}`)
}
