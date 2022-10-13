export const onRequestPost = async ({ env, data }) => {
  const id = env.EB_LEDGER_DO.newUniqueId();
  var ledgerList = await env.EB.get(`user:${data.auth.sub}:ledgers`, { type: "json" }) || [];
  ledgerList.push(id);
  env.EB.put(`user:${data.auth.sub}:ledgers`, ledgerList);
  return new Response(JSON.stringify(id.toString()), { status: 201});
}

export const onRequestGet = async ({ env, data }) => {
  var ledgerList = await env.EB.get(`user:${data.auth.sub}:ledgers`, { type: "json" }) || [];
  return new Response(JSON.stringify(ledgerList));
}