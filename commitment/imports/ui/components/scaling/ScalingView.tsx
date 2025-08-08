"use client"

import React, { useState, useEffect } from 'react';
import ScalingConfigForm from './ScalingConfigForm';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '../ui/dialog';

import { Button } from '../ui/button';
import { GradingSheetForm } from './GradingSheetForm';
import ScalingSummary from './ScalingSummary';

interface ScalingConfig {
  metrics: string[];
  method: string;
  customScript?: File[];
}

function ScalingView() {
  const [completed, setCompleted] = useState(false);
  const [hasLoaded, setHasLoaded] = useState(false);
  const [step, setStep] = useState<'config' | 'sheet' | 'done'>('config');
  const [showDialog, setShowDialog] = useState(false);
  const [dialogClosedManually, setDialogClosedManually] = useState(false);
  const [config, setConfig] = useState<ScalingConfig | null>(null);
  const [gradingSheet, setGradingSheet] = useState<File | null>(null);

  // Load from localStorage on first mount
  useEffect(() => {
    const lsCompleted = localStorage.getItem('hasVisitedScaling') === 'true';
    setCompleted(lsCompleted);
    if (!lsCompleted) setShowDialog(true);
    setHasLoaded(true);
  }, []);

  // Persist to localStorage
  useEffect(() => {
    if (hasLoaded) {
      localStorage.setItem('hasVisitedScaling', completed ? 'true' : 'false');
    }
  }, [completed, hasLoaded]);

  const handleConfigSubmit = (configData: ScalingConfig) => {
    setConfig(configData);
    setStep('sheet');
  };

  const handleSheetSubmit = (sheetFile: File) => {
    setGradingSheet(sheetFile);
    setCompleted(true);
    setShowDialog(false);
    setStep('done');
  };

  return (
    <div className="m-0 scroll-smooth">
      <div className="flex flex-col gap-32">
        <div className="max-w-[1600px] mx-20 rounded-2xl bg-white p-8">
          
        {hasLoaded && !completed && !showDialog && (
        <Button
            className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
            onClick={() => {
            setStep('config');
            setShowDialog(true);
            }}
        >
            Create New Scaling
        </Button>
        )}

          {/* Show summary if completed */}
          {completed && config && gradingSheet && (
           <ScalingSummary 
                method={config.method} 
                metrics={config.metrics.join(', ')} 
                fileName={gradingSheet.name} 
            />
            // <div className="mt-8 p-4 border rounded-md bg-gray-50">
            //   <h2 className="text-xl font-semibold mb-2">Scaling Summary</h2>
            //   <p><strong>Method:</strong> {config.method}</p>
            //   <p><strong>Metrics:</strong> {config.metrics.join(', ')}</p>
            //   <p><strong>Grading Sheet:</strong> {gradingSheet.name}</p>
            // </div>
          )}

          {/* Multi-Step Dialog */}
          <Dialog
            open={showDialog}
            onOpenChange={(open) => {
              setShowDialog(open);
              if (!open) {
                setDialogClosedManually(true); // track if user manually closed
              }
            }}
          >
            <DialogHeader>
              <DialogTitle />
              <DialogDescription />
            </DialogHeader>
            <DialogContent className="max-w-full">
              {step === 'config' && (
                <ScalingConfigForm onSubmit={handleConfigSubmit} />
              )}
              {step === 'sheet' && (
                <GradingSheetForm onSubmit={handleSheetSubmit} />
              )}
            </DialogContent>
          </Dialog>
        </div>
      </div>
    </div>
  );
}

export default ScalingView;
