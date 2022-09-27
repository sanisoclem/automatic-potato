export const onRequestPut = async ({ env, params }) => {
  const doId = env.EB_LEDGER_DO.idFromString(params.id);
  const stub = env.EB_LEDGER_DO.get(doId);
  return stub.fetch(request);
}

export const onRequestGet = async ({ env, params, request }) => {
  const url = new URL(request.url);
  const segments = params.id.slice(1).split("/")
  const doId = env.EB_LEDGER_DO.idFromString(segments[0]);
  const stub = env.EB_LEDGER_DO.get(doId);
  url.pathname = `/${segments[1]}`
  return stub.fetch(new Request(url));
}
