
export const onRequest = async ({ data, request, env, params }) => {
  if (request.method === "POST") {
    const id = env.EB_DATA.newUniqueId();
    const key = getLedgerListKey(data.auth.sub);
    const ledgerList = (await env.EB.get(key, { type: "json" })) || [];
    ledgerList.push(id.toString());
    await env.EB.put(key, JSON.stringify(ledgerList));
    return new Response(JSON.stringify(id.toString()), { status: 201 });
  } else if (request.method === "PUT") {
    const doId = env.EB_DATA.idFromString(params.path[0]);
    const stub = env.EB_DATA.get(doId);
    return await stub.fetch(request);
  } else if (request.method === "GET") {
    if (!params.path) {
      const ledgerIds = (await env.EB.get(getLedgerListKey(data.auth.sub), { type: "json" })) || [];
      return new Response(JSON.stringify(ledgerIds));
    } else {
      const url = new URL(request.url);
      const doId = env.EB_DATA.idFromString(params.path[0]);
      const stub = env.EB_DATA.get(doId);
      url.pathname = `/${params.path[1]}`;
      return await stub.fetch(new Request(url));
    }
  } else {
    return new Response("Method Not Allowed", { status: 405 });
  }
};

const getLedgerListKey = (user) => `u:${user}:ledgers`;
