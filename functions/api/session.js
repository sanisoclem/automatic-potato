export const onRequestGet = async ({ data }) => {
  return new Response(JSON.stringify({ payload: data.auth }));
};
