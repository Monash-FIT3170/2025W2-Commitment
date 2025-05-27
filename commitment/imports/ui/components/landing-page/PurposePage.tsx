import React from "react";
import { Separator } from "@ui/components/ui/separator";
import { Card, CardContent } from "@ui/components/ui/card";
import GetStartedButton from "./getStartedButton";

const purpose = () => {
  return (
    <div className="flex flex-col justify-center items-center mt-20">
      <Header />

      <CircleNumber />

      <PurposeCard />
      <GetStartedButton className="mt-12"/>
    </div>
  );
};

export default purpose;

const Header = () => {
  return (
    <div className="flex flex-col justify-center items-center">
      <h4 className="font-mono text text-git font-semibold">HOW TO USE?</h4>
      <h1 className="font-mono text-3xl">Get Started in Three Steps</h1>
    </div>
  );
};

const CircleNumber = () => {
  return (
    <div className="flex flex-row pt-7 pb-3 items-center gap-2 justify-center">
      <div className="font-mono w-12 h-12 bg-[#D9D9D9] rounded-full flex justify-center items-center text-center p-5 text-xl">
        1
      </div>
      <Separator decorative className="bg-black"/>
      <div className="font-mono w-12 h-12 bg-[#D9D9D9] rounded-full flex justify-center items-center text-center p-5 text-xl">
        2
      </div>
      <Separator decorative className="bg-black"/>
      <div className="font-mono w-12 h-12 bg-[#D9D9D9] rounded-full flex justify-center items-center text-center p-5 text-xl">
        3
      </div>
    </div>
  );
};

const PurposeCard = () => {
  return (
    <Card className="max-w-4xl shadow-md pt-2">
      <CardContent className="grid grid-row-2 gap-4 text-center pt-4">
        <div className="grid grid-cols-3 gap-4 items-center justify-items-center">
          <img src="/github_logo.svg" alt="Github Logo" className="max-h-32" />
          <img
            src="/git_search_bar.svg"
            alt="Commitment Search Bar"
            className="shadow-md rounded-[52px]"
          />
          <img
            src="/num_of_commit_box.svg"
            alt="Number of commit box"
            className="max-h-32"
          />
        </div>

        <div className="font-mono grid grid-cols-3 gap-4 items-start justify-items-center pt1 italic font-light">
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
};
