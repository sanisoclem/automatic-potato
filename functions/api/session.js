import * as jose from 'jose'

const getCookie = (cookieString, key) => {
  if (cookieString) {
    const allCookies = cookieString.split("; ")
    const targetCookie = allCookies.find(cookie => cookie.includes(key))
    if (targetCookie) {
      const [_, value] = targetCookie.split("=")
      return value
    }
  }

  return null
}

export const onRequestGet = async ({ env, request }) => {
  const cookies = request.headers.get('Cookie');
  const token = getCookie(cookies, 'authToken');

  try {
    const { payload } = await jose.jwtVerify(token, await jose.importSPKI(env.AUTH_JWT_PUBLIC_KEY, 'ES384'), {
      issuer: env.AUTH_JWT_ISSUER,
      audience: env.AUTH_JWT_AUDIENCE,
    });

    return new Response(JSON.stringify({
      payload
    }));
  }
  catch {
    return new Response(JSON.stringify({}));
  }
}


