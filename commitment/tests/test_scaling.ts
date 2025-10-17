import { Meteor } from "meteor/meteor";
import { scaleUsers } from "/server/ScalingFunctions";

Meteor.startup(async () => {
  const fakeMetrics = {
    alice: { commits: 10, loc: 200 },
    bob: { commits: 30, loc: 400 },
    charlie: { commits: 50, loc: 600 },
  };

  const config = {
    method: "Compact Scaling",
    metrics: ["commits", "loc"],
  };

  // @ts-ignore — manual test
  const result = await scaleUsers("dummyUrl", config);
  console.log("Compact Scaling Test:", result);
});
