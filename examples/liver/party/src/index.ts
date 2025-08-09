import { server } from "../../output-es/Liver/index.js";
import { routePartykitRequest } from "partyserver";

export const Live = server();

export default {
  async fetch(request: Request, env: any): Promise<Response> {
    let resp = await routePartykitRequest(request, { ...env });
    return (
      resp ||
      env.ASSETS.fetch(request) ||
      new Response("Not Found", { status: 404 })
    );
  },
};
