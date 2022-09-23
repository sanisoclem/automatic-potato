import * as jose from 'jose'

export const onRequestGet = async ({ env, request }) => {
  const token = request.headers.get('Authorization').split(' ')[1];

  const { payload, protectedHeader } = await jose.jwtVerify(token, await jose.importSPKI(env.AUTH_JWT_PUBLIC_KEY, 'ES384'), {
    issuer: env.AUTH_JWT_ISSUER,
    audience: env.AUTH_JWT_AUDIENCE,
  });

  return new Response(JSON.stringify({
    payload,
    protectedHeader
  }));
}
