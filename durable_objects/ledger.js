
import { ledgerFetchMain } from '../output/AP.DurableObject.EntryPoint';

export class Ledger {
  constructor(state) {
    this.state = state;
  }

  async fetch(request) {
    return await ledgerFetchMain(this.state.storage)(request)();
  }
}

