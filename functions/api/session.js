export const onRequestGet = async ({ data }) => {
  return new Response(JSON.stringify(data.auth));
};
