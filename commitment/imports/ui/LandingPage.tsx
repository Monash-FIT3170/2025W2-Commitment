import React from "react";
import NavBar from "@ui/components/landing-page/NavBar";

export const LandingPage = () => (
  <div className="m-9">
    <h1 className="text-cyan-700 font-sans text-5xl m-9 text-center font-thin">
      Commitment
    </h1>
    <div className="flex flex-col gap-6">
      {/* Put components here */}
      <NavBar />
    </div>
  </div>
);
