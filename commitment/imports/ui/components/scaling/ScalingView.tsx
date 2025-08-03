import React from "react";

import { ScalingConfigForm } from "./ScalingConfigForm";
import { useState } from "react";

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

    return(
        <div>
            <ScalingConfigForm onSubmit={handleConfigSubmit}/>
        </div>
    );
}