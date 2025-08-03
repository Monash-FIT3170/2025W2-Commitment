import React from "react";

import { ScalingConfigForm } from "./ScalingConfigForm";
import { useState } from "react";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from "../ui/dialog";

import { Button } from "../ui/button";

interface ScalingConfig {
  metrics: string[];
  method: string;
  customScript?: File;
}

export const ScalingView = () => {
  const [step, setStep] = useState<1 | 2 | 3>(1);

  // Shared state
  const [config, setConfig] = useState<ScalingConfig | null>(null);
  const [gradingSheet, setGradingSheet] = useState<File | null>(null);
  const handleConfigSubmit = (configData: ScalingConfig) => {
    setConfig(configData);
    setStep(2);
  };

  return (
    <div className="m-0 scroll-smooth">
      <div className="flex flex-col gap-32">
        <div className="max-w-[1600px] mx-20 rounded-2xl bg-white p-8">
          {/* TRIGGER FOR SCALING WIZARD */}
          <Dialog>
            <DialogTrigger asChild>
              <Button className="bg-orange-400"> Generate New Scaling</Button>
            </DialogTrigger>
            <DialogContent>
              <DialogHeader>
                <DialogTitle>Scaling Configuration</DialogTitle>
              </DialogHeader>
              <ScalingConfigForm
                onSubmit={(data) => {
                  console.log("Submitted Config", data);
                }}
              />
            </DialogContent>
          </Dialog>
        </div>
      </div>
    </div>
  );
};
