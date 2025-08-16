import React from "react";
import GetStartedButton from "./getStartedButton";

const MainPage = () => (
  <div className="flex flex-col gap-6 mb-32 ">
    <div className="flex justify-center items-center mb-4">
      <Logo />
    </div>

    <div className="flex justify-center items-center mb-6">
      <GetStartedButton />
    </div>

    <div className="flex justify-center items-center">
      <Description />
    </div>
  </div>
);

export const Logo = () => (
  <div className="flex flex-row items-center text-center">
    <img src="/logo.png" alt="Logo" className="-mr-7 w-[120px] h-auto" />
    <p className="font-mono text-8xl">Commitment</p>
  </div>
);

const Description = () => (
  <div className="text-center">
    <p className="font-mono text-center flex">
      Need to quickly understand how committed your students are to their group
      projects?
    </p>
  </div>
);

export default MainPage;
