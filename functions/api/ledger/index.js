export const onRequestPost = async ({ env }) => {
  const id = env.EB_LEDGER_DO.newUniqueId();
  var ledgerList = env.eb.get(`user:${data.auth.sub}:ledgers`, { type: "json" }) || [];
  ledgerList.push(id);
  env.EB.put(`user:${data.auth.sub}:ledgers`, ledgerList);
  return new Response(id.toString(), { status: 201});
}