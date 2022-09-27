import * as jose from "jose";

const getCookie = (cookieString, key) => {
  if (cookieString) {
    const allCookies = cookieString.split("; ");
    const targetCookie = allCookies.find((cookie) => cookie.includes(key));
    if (targetCookie) {
      const [_, value] = targetCookie.split("=");
      return value;
    }
  }

  return null;
};

export const onRequest = [
  async (context) => {
    context.data.timestamp = Date.now();
    const res = await context.next();
    let delta = Date.now() - context.data.timestamp;
    res.headers.set("x-response-timing", delta);
    return res;
  },
  async (context) => {
    try {
      return await context.next();
    } catch (err) {
      return new Response(`${err.message}\n${err.stack}`, { status: 500 });
    }
  },
  async ({ request, next, data, env }) => {
    var url = new URL(request.url);

    if (!url.pathname.startsWith("/api/auth/")) {
      try {
        const cookies = request.headers.get("Cookie");
        const token = getCookie(cookies, "authToken");

        if (!token) {
          return new Response("Unauthorized", { status: 401 });
        }

        const { payload } = await jose.jwtVerify(
          token,
          await jose.importSPKI(env.AUTH_JWT_PUBLIC_KEY, "ES384"),
          {
            issuer: env.AUTH_JWT_ISSUER,
            audience: env.AUTH_JWT_AUDIENCE,
          }
        );

        data.auth = payload;
      } catch {
        return new Response("Unauthorized", { status: 403 });
      }
    }

    return await next();
  },
];
