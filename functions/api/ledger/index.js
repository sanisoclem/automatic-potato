export const onRequestPost = async ({ env, data }) => {
  const id = env.EB_LEDGER_DO.newUniqueId();
  var ledgerList = env.EB.get(`user:${data.auth.sub}:ledgers`, { type: "json" }) || [];
  ledgerList.push(id);
  env.EB.put(`user:${data.auth.sub}:ledgers`, ledgerList);
  return new Response(id.toString(), { status: 201});
}
