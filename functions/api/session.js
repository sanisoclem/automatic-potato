import { ledgerFetchMain } from '../../output/AP.DurableObject.EntryPoint';

export const onRequestGet = async ({ data }) => {
  return new Response(JSON.stringify(data.auth));
};


export class Ledger {
  constructor(state) {
    this.state = state;
  }

  async fetch(request) {
    return await ledgerFetchMain(this.state.storage)(request)();
  }
}

