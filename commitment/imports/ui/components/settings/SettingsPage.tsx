import React, { useState, useRef } from 'react';
import NavBar from '@ui/components/landing-page/NavBar';
import { Meteor } from 'meteor/meteor';
import { useTracker } from 'meteor/react-meteor-data';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@ui/components/ui/tabs';
import { Button } from '@ui/components/ui/button';
import { Upload, Info, FileText, X } from 'lucide-react';

export const SettingsPage: React.FC = () => {
  const user = useTracker(() => Meteor.user());
  const isLoggedIn = !!user;
  
  const [activeTab, setActiveTab] = useState('profile');
  
  // File upload state
  const [isDragOver, setIsDragOver] = useState(false);
  const [selectedFile, setSelectedFile] = useState<File | null>(null);
  const [fileContent, setFileContent] = useState<string>('');
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string>('');
  
  // Reference to the hidden file input
  const fileInputRef = useRef<HTMLInputElement>(null);

  // Handle file selection from button click
  const handleFileSelect = () => {
    fileInputRef.current?.click();
  };

  // Handle file input change
  const handleFileChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (file) {
      processFile(file);
    }
  };

  // Handle drag and drop events
  const handleDragOver = (e: React.DragEvent) => {
    e.preventDefault();
    setIsDragOver(true);
  };

  const handleDragLeave = (e: React.DragEvent) => {
    e.preventDefault();
    setIsDragOver(false);
  };

  const handleDrop = (e: React.DragEvent) => {
    e.preventDefault();
    setIsDragOver(false);
    
    const file = e.dataTransfer.files[0];
    if (file) {
      processFile(file);
    }
  };

  // Process the uploaded file
  const processFile = (file: File) => {
    // Reset previous state
    setError('');
    setFileContent('');
    
    // Basic file validation
    if (file.type !== 'application/json' && !file.name.endsWith('.json')) {
      setError('Please upload a JSON file (.json)');
      return;
    }
    
    if (file.size > 5 * 1024 * 1024) { // 5MB limit
      setError('File size must be less than 5MB');
      return;
    }

    setSelectedFile(file);
    setIsLoading(true);

    // Read file content
    const reader = new FileReader();
    reader.onload = (e) => {
      try {
        const content = e.target?.result as string;
        // Try to parse JSON to validate it
        JSON.parse(content);
        setFileContent(content);
        setError('');
      } catch (parseError) {
        setError('Invalid JSON file. Please check the file format.');
        setSelectedFile(null);
      } finally {
        setIsLoading(false);
      }
    };
    
    reader.onerror = () => {
      setError('Error reading file. Please try again.');
      setIsLoading(false);
    };

    reader.readAsText(file);
  };

  // Remove selected file
  const removeFile = () => {
    setSelectedFile(null);
    setFileContent('');
    setError('');
    if (fileInputRef.current) {
      fileInputRef.current.value = '';
    }
  };

  return (
    <div className="min-h-screen bg-git-bg-primary">
      <NavBar isLoggedIn={isLoggedIn} />
      
      <div className="max-w-6xl mx-auto px-6 py-8 pt-24">
        <h1 className="text-4xl font-bold text-git-text-primary mb-4">
          Settings
        </h1>
        <p className="text-git-text-secondary text-lg mb-8">
          This is the settings page. Content coming soon!
        </p>

        {/* This is the tab system - using EXACT same styling as MetricsTab */}
        <Tabs value={activeTab} onValueChange={setActiveTab} className="w-full bg-[#FEFEFA] shadow-sm justify-items-start">
          {/* This creates the tab buttons at the top - same styling as MetricsTab */}
          <TabsList className="flex bg-[#FEFEFA] border-b">
            <TabsTrigger 
              value="profile" 
              className={`
                relative px-4 text-lg font-medium text-gray-600
                bg-[#FEFEFA] hover:bg-[#D0D0B7]
                data-[state=active]:bg-gray-100
                data-[state=active]:text-gray-900
                data-[state=active]:after:content-['']
                data-[state=active]:after:absolute
                data-[state=active]:after:bottom-0
                data-[state=active]:after:left-0
                data-[state=active]:after:w-full
                data-[state=active]:after:h-0.5
                data-[state=active]:after:bg-git
                rounded-none border-none shadow-none focus:outline-hidden
                transition-all
              `}
            >
              Profile
            </TabsTrigger>
            <TabsTrigger 
              value="alias-config" 
              className={`
                relative px-4 text-lg font-medium text-gray-600
                bg-[#FEFEFA] hover:bg-[#D0D0B7]
                data-[state=active]:bg-gray-100
                data-[state=active]:text-gray-900
                data-[state=active]:after:content-['']
                data-[state=active]:after:absolute
                data-[state=active]:after:bottom-0
                data-[state=active]:after:left-0
                data-[state=active]:after:w-full
                data-[state=active]:after:h-0.5
                data-[state=active]:after:bg-git
                rounded-none border-none shadow-none focus:outline-hidden
                transition-all
              `}
            >
              Alias Configuration
            </TabsTrigger>
          </TabsList>

          {/* This is the content for the Profile tab */}
          <TabsContent value="profile" className="mt-6">
            <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg">
              <h2 className="text-2xl font-semibold text-git-text-primary mb-4">Profile Settings</h2>
              <p className="text-git-text-secondary">Profile settings will go here!</p>
            </div>
          </TabsContent>

          {/* This is the content for the Alias Configuration tab */}
          <TabsContent value="alias-config" className="mt-6">
            <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg">
              <div className="mb-6">
                <div className="flex items-center gap-2 mb-2">
                  <h2 className="text-2xl font-semibold text-git-text-primary">Upload config file</h2>
                  <Info className="h-5 w-5 text-git-text-secondary" />
                </div>
                <p className="text-git-text-secondary">Map multiple accounts to a single user.</p>
              </div>

              {/* Hidden file input */}
              <input
                ref={fileInputRef}
                type="file"
                accept=".json"
                onChange={handleFileChange}
                className="hidden"
              />

              {/* Upload Area */}
              <div 
                className={`border-2 border-dashed rounded-lg p-8 text-center transition-colors ${
                  isDragOver 
                    ? 'border-git-int-primary bg-git-int-primary/10' 
                    : 'border-git-stroke-secondary bg-git-bg-secondary'
                }`}
                onDragOver={handleDragOver}
                onDragLeave={handleDragLeave}
                onDrop={handleDrop}
              >
                {!selectedFile ? (
                  <>
                    <Upload className="h-12 w-12 text-git-text-secondary mx-auto mb-4" />
                    <h3 className="text-lg font-semibold text-git-text-primary mb-2">
                      Drag and drop your files here
                    </h3>
                    <p className="text-git-text-secondary mb-6">File format accepted: JSON</p>
                    
                    <Button 
                      variant="secondary" 
                      size="lg"
                      className="bg-git-bg-tertiary hover:bg-git-bg-elevated text-git-text-primary border border-git-stroke-primary"
                      onClick={handleFileSelect}
                      disabled={isLoading}
                    >
                      <Upload className="h-4 w-4" />
                      {isLoading ? 'Processing...' : 'Upload files'}
                    </Button>
                  </>
                ) : (
                  <div className="text-center">
                    <FileText className="h-12 w-12 text-git-int-primary mx-auto mb-4" />
                    <h3 className="text-lg font-semibold text-git-text-primary mb-2">
                      {selectedFile.name}
                    </h3>
                    <p className="text-git-text-secondary mb-4">
                      File size: {(selectedFile.size / 1024).toFixed(1)} KB
                    </p>
                    <Button 
                      variant="outline"
                      size="sm"
                      onClick={removeFile}
                      className="border-git-stroke-primary text-git-text-primary hover:bg-git-bg-elevated"
                    >
                      <X className="h-4 w-4 mr-2" />
                      Remove file
                    </Button>
                  </div>
                )}
              </div>

              {/* Error Display */}
              {error && (
                <div className="mt-4 p-3 bg-red-50 border border-red-200 rounded-lg">
                  <p className="text-red-600 text-sm">{error}</p>
                </div>
              )}

              {/* File Content Preview */}
              {fileContent && (
                <div className="mt-6">
                  <h3 className="text-lg font-semibold text-git-text-primary mb-3">File Preview:</h3>
                  <div className="bg-git-bg-secondary border border-git-stroke-secondary rounded-lg p-4">
                    <pre className="text-sm text-git-text-secondary overflow-auto max-h-40">
                      {fileContent}
                    </pre>
                  </div>
                </div>
              )}

              {/* Apply Button */}
              <div className="mt-6 text-center">
                <Button 
                  size="lg"
                  className="bg-git-int-primary hover:bg-git-int-primary-hover text-git-int-text px-8"
                  disabled={!selectedFile || !!error}
                >
                  Apply
                </Button>
              </div>
            </div>
          </TabsContent>
        </Tabs>
      </div>
    </div>
  );
};

export default SettingsPage;
