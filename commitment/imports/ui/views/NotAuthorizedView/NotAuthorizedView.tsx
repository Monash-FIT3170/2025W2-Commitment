import React from "react";
import { Button } from "@ui/components/ui/button";

export default function NotAuthorizedView() {
  return (
    <div className="w-full h-screen  flex flex-col">

      <div className="flex-grow flex flex-row w-full h-full justify-center p-3">
        <div className="flex flex-col gap-3 justify-center">
          <div className="text-2xl pb-3">Something went wrong...</div>
          <div className="">You need to log in to access this page.</div>

          <div className="grid grid-cols-2 auto-cols-fr gap-3 mt-9">
            <Button asChild variant="outline">
              <a href="/login">Log in</a>
            </Button>
            <Button asChild>
              <a href="/Sign up">Sign up</a>
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}
