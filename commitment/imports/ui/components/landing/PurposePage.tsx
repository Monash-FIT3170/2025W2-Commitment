import React from "react";
import { Separator } from "@base/separator";
import { Card, CardContent } from "@base/card";
import GetStartedButton from "./getStartedButton";

const Header = () => (
  <div className="flex flex-col justify-center items-center">
    <h4 className="font-mono text text-git-500 text-lg font-semibold tracking-wide">
      HOW TO USE?
    </h4>
    <h1 className="font-mono text-5xlx">Get Started in Three Steps</h1>
  </div>
);

function CircleNumber() {
  return (
    <div className="flex flex-row pt-7 pb-3 items-center gap-2 justify-center">
      <div className="font-mono w-12 h-12 bg-git-bg-tertiary rounded-full flex justify-center items-center text-center p-5 text-xl">
        1
      </div>
      <Separator decorative className="bg-black" />
      <div className="font-mono w-12 h-12 bg-git-bg-tertiary rounded-full flex justify-center items-center text-center p-5 text-xl">
        2
      </div>
      <Separator decorative className="bg-black" />
      <div className="font-mono w-12 h-12 bg-git-bg-tertiary rounded-full flex justify-center items-center text-center p-5 text-xl">
        3
      </div>
    </div>
  );
}

function PurposeCard() {
  return (
    <Card className="max-w-4xl shadow-md pt-2 bg-git-bg-elevated">
      <CardContent className="grid grid-row-2 gap-4 text-center pt-4">
        <div className="grid grid-cols-3 gap-4 items-center justify-items-center">
          <img src="/github_logo.svg" alt="Github Logo" className="max-h-32" />
          <img
            src="/insert_light.png"
            alt="Commitment Search Bar"
            className="shadow-md rounded-[52px] block dark:hidden"
          />
          <img
            src="/insert_dark.png"
            alt="Commitment Search Bar Dark"
            className="shadow-md rounded-[52px] hidden dark:block"
          />
          <img
            src="/graph_light.png"
            alt="Graph of contributions"
            className="max-h-32 block dark:hidden"
          />
          <img
            src="/graph_dark.png"
            alt="Graph of contributions dark"
            className="max-h-32 hidden dark:block"
          />
        </div>

        <div className="font-mono grid grid-cols-3 gap-4 items-start justify-items-center pt1 italic font-semibold text-git-text-primary dark:text-git-text-secondary">
          <div>
            Find the
            <br />
            repository link
            <br />
            on GitHub
          </div>
          <div>
            Enter your
            <br />
            repository
          </div>
          <div>
            Analyse the
            <br />
            contributor
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

const purpose = () => (
  <div className="flex flex-col justify-center items-center mt-20">
    <Header />

    <CircleNumber />

    <PurposeCard />

    <div className="mt-20">
      <GetStartedButton />
    </div>
  </div>
);

export default purpose;
