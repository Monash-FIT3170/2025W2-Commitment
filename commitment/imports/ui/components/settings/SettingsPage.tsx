import React, { useState, useRef, useEffect } from 'react';
import { Meteor } from 'meteor/meteor';
import { useTracker } from 'meteor/react-meteor-data';
import { Button } from '@ui/components/ui/button';
import { Upload, FileText, X, CheckCircle, AlertCircle, Download, Play, ArrowLeft } from 'lucide-react';
import InfoButton from '../ui/infoButton';
import { useNavigate } from 'react-router-dom';

export const AliasConfigPage: React.FC = () => {
  const user = useTracker(() => Meteor.user());
  const isLoggedIn = !!user;
  const navigate = useNavigate();
  
  // Repository metrics navigation state
  const [hasVisitedRepo, setHasVisitedRepo] = useState(false);
  const [lastRepoUrl, setLastRepoUrl] = useState<string | null>(null);
  
  // File upload state
  const [isDragOver, setIsDragOver] = useState(false);
  const [selectedFile, setSelectedFile] = useState<File | null>(null);
  const [fileContent, setFileContent] = useState<string>('');
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string>('');
  
  // Config state
  const [parsedAliases, setParsedAliases] = useState<any>(null);
  const [showConfirmation, setShowConfirmation] = useState(false);
  const [isSaving, setIsSaving] = useState(false);
  const [successMessage, setSuccessMessage] = useState('');
  
  // Current config state
  const [currentConfigs, setCurrentConfigs] = useState<any[]>([]);
  const [isLoadingConfigs, setIsLoadingConfigs] = useState(false);
  
  // Reference to the hidden file input
  const fileInputRef = useRef<HTMLInputElement>(null);

  // Check if user has visited a repository
  useEffect(() => {
    const rememberedRepoUrl = localStorage.getItem('lastRepoUrl');
    if (rememberedRepoUrl) {
      setHasVisitedRepo(true);
      setLastRepoUrl(rememberedRepoUrl);
    } else {
      setHasVisitedRepo(false);
      setLastRepoUrl(null);
    }
  }, []);

  // Function to navigate back to repository metrics
  const handleBackToMetrics = () => {
    if (lastRepoUrl) {
      navigate("/metrics", { 
        state: { 
          repoUrl: lastRepoUrl,
          tab: "metrics" 
        } 
      });
    }
  };


  // Load current configs
  const loadCurrentConfigs = async () => {
    if (!isLoggedIn) return;
    
    setIsLoadingConfigs(true);
    try {
      const configs = await Meteor.callAsync('aliasConfigs.getAllForOwner');
      setCurrentConfigs(configs);
    } catch (error) {
      console.error('Error loading configs:', error);
    } finally {
      setIsLoadingConfigs(false);
    }
  };

  // Remove a config
  const removeConfig = async (configId: string) => {
    if (!isLoggedIn) return;
    
    try {
      await Meteor.callAsync('aliasConfigs.remove', configId);
      setSuccessMessage('Config removed successfully!');
      loadCurrentConfigs(); // Reload the list
    } catch (error) {
      setError(`Failed to remove config: ${  (error as any).message}`);
    }
  };

  // Load configs when component mounts
  React.useEffect(() => {
    loadCurrentConfigs();
  }, [isLoggedIn]);

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
    setParsedAliases(null);
    setShowConfirmation(false);
    setSuccessMessage('');
    
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
        const parsed = JSON.parse(content);
        
        // Validate the structure
        const validationResult = validateConfigStructure(parsed);
        if (!validationResult.isValid) {
          setError(validationResult.error || 'Invalid configuration structure');
          setSelectedFile(null);
          return;
        }
        
        setFileContent(content);
        setParsedAliases(parsed);
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

  // Validate config structure
  const validateConfigStructure = (data: any): { isValid: boolean; error?: string } => {
    // Check if data has required top-level properties
    if (!data.name || typeof data.name !== 'string') {
      return { isValid: false, error: 'Config must have a "name" field (string)' };
    }
    
    if (!data.aliases || !Array.isArray(data.aliases)) {
      return { isValid: false, error: 'Config must have an "aliases" field (array)' };
    }
    
    if (data.aliases.length === 0) {
      return { isValid: false, error: 'Config must have at least one student alias' };
    }
    
    // Validate each alias
    for (let i = 0; i < data.aliases.length; i++) {
      const alias = data.aliases[i];
      const aliasIndex = i + 1;
      
      if (!alias.officialName || typeof alias.officialName !== 'string') {
        return { isValid: false, error: `Alias ${aliasIndex}: missing or invalid "officialName"` };
      }
      
      if (!alias.gitUsernames || !Array.isArray(alias.gitUsernames)) {
        return { isValid: false, error: `Alias ${aliasIndex}: missing or invalid "gitUsernames" array` };
      }
      
      if (alias.gitUsernames.length === 0) {
        return { isValid: false, error: `Alias ${aliasIndex}: "gitUsernames" array cannot be empty` };
      }
      
      if (!alias.emails || !Array.isArray(alias.emails)) {
        return { isValid: false, error: `Alias ${aliasIndex}: missing or invalid "emails" array` };
      }
      
      if (alias.emails.length === 0) {
        return { isValid: false, error: `Alias ${aliasIndex}: "emails" array cannot be empty` };
      }
      
      // Validate array contents are strings
      if (!alias.gitUsernames.every((u: any) => typeof u === 'string')) {
        return { isValid: false, error: `Alias ${aliasIndex}: "gitUsernames" must contain only strings` };
      }
      
      if (!alias.emails.every((e: any) => typeof e === 'string')) {
        return { isValid: false, error: `Alias ${aliasIndex}: "emails" must contain only strings` };
      }
    }
    
    return { isValid: true };
  };

  // Remove selected file
  const removeFile = () => {
    setSelectedFile(null);
    setFileContent('');
    setParsedAliases(null);
    setShowConfirmation(false);
    setError('');
    setSuccessMessage('');
    if (fileInputRef.current) {
      fileInputRef.current.value = '';
    }
  };

  // Handle Apply button click
  const handleApply = () => {
    if (!parsedAliases) {
      setError('Please upload and validate a config file first');
      return;
    }
    
    setShowConfirmation(true);
  };

  // Handle final confirmation and save
  const handleConfirmSave = async () => {
    if (!parsedAliases) return;
    
    setIsSaving(true);
    setError('');
    
    try {
      // Call Meteor method to save config
      await new Promise((resolve, reject) => {
        Meteor.call('aliasConfigs.create', parsedAliases.name, parsedAliases.aliases, (error: any, result: any) => {
          if (error) {
            reject(error);
          } else {
            resolve(result);
          }
        });
      });
      
      // Success!
      setSuccessMessage('Configuration saved successfully!');
      setShowConfirmation(false);
      loadCurrentConfigs(); // Reload the config list
      
      // Reset form after a delay
      setTimeout(() => {
        removeFile();
      }, 2000);
      
    } catch (error: any) {
      setError(error.message ? error.message : 'Failed to save configuration');
      setShowConfirmation(false);
    } finally {
      setIsSaving(false);
    }
  };

  // Cancel confirmation
  const handleCancelSave = () => {
    setShowConfirmation(false);
  };

  // Download sample template
  const downloadTemplate = () => {
    const template = {
      name: "Monash Sem 2 2025",
      aliases: [
        {
          officialName: "John Smith",
          gitUsernames: ["johnsmith", "jsmith_dev"],
          emails: ["john.smith@university.edu", "jsmith@github.com"]
        },
        {
          officialName: "Sarah Johnson",
          gitUsernames: ["sarahj", "sarah_dev"],
          emails: ["sarah.johnson@university.edu", "sarahj@github.com"]
        },
        {
          officialName: "Michael Chen",
          gitUsernames: ["mchen", "michael_c"],
          emails: ["michael.chen@university.edu", "mchen@github.com"]
        }
      ]
    };

    const blob = new Blob([JSON.stringify(template, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'alias-config-template.json';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  };

  return (
    <div className="min-h-screen bg-git-bg-primary">
      <div className="max-w-6xl mx-auto px-6 py-8">
        <div className="flex items-center justify-between mb-4">
          <h1 className="text-4xl font-bold text-git-text-primary">
            Alias Configuration
          </h1>
          {hasVisitedRepo && lastRepoUrl && (
            <div className="relative group">
              <Button
                onClick={handleBackToMetrics}
                variant="outline"
                disabled={currentConfigs.length === 0}
                className="flex items-center gap-2"
              >
                <ArrowLeft className="h-4 w-4" />
                Back to Repository Metrics
              </Button>
              {currentConfigs.length === 0 && (
                <div className="absolute bottom-full left-1/2 transform -translate-x-1/2 mb-2 px-3 py-2 bg-gray-900 text-white text-sm rounded-lg opacity-0 group-hover:opacity-100 transition-opacity duration-200 pointer-events-none whitespace-nowrap z-10">
                  Upload a config file to enable this feature
                  <div className="absolute top-full left-1/2 transform -translate-x-1/2 border-4 border-transparent border-t-gray-900"></div>
                </div>
              )}
            </div>
          )}
        </div>
        <p className="text-git-text-secondary text-lg mb-8">
          Manage student alias mappings to consolidate contributions across multiple Git identities.
        </p>

        {/* Alias Configuration Section */}
        <div className="space-y-6">
            {/* Current Config Section */}
            <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg mb-6">
              <div className="mb-4">
                <div className="flex items-center gap-2 mb-2">
                  <h2 className="text-2xl font-semibold text-git-text-primary">Your Alias Configuration</h2>
                  <InfoButton description="You can have one universal config file that applies to all repositories you analyse." />
                </div>
              </div>

              {isLoadingConfigs ? (
                <div className="text-center py-8">
                  <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-git-accent-primary mx-auto mb-4" />
                  <p className="text-git-text-secondary">Loading your configuration...</p>
                </div>
              ) : currentConfigs.length === 0 ? (
                <div className="text-center py-8">
                  <div className="w-16 h-16 bg-git-bg-secondary rounded-full flex items-center justify-center mx-auto mb-4">
                    <FileText className="h-8 w-8 text-git-text-secondary" />
                  </div>
                  <h3 className="text-lg font-medium text-git-text-primary mb-2">No Configuration Set</h3>
                </div>
              ) : (
                <div className="space-y-4">
                  {currentConfigs.map((config) => (
                    <div key={config._id} className="p-6 bg-git-bg-primary border-2 border-git-accent-primary rounded-lg">
                      <div className="flex justify-between items-start mb-4">
                        <div className="flex-1">
                          <div className="flex items-center gap-2 mb-2">
                            <CheckCircle className="h-5 w-5 text-green-500" />
                            <h3 className="text-lg font-semibold text-git-text-primary">{config.name}</h3>
                            <span className="px-2 py-1 bg-green-100 text-green-800 text-xs rounded-full">
                              Active
                            </span>
                          </div>
                          <p className="text-sm text-git-text-secondary">
                            Created: {new Date(config.createdAt).toLocaleDateString()} • {config.aliases.length} students mapped
                          </p>
                        </div>
                        <Button
                          variant="outline"
                          size="sm"
                          onClick={() => removeConfig(config._id)}
                          className="text-red-600 border-red-200 hover:bg-red-50"
                        >
                          <X className="h-4 w-4 mr-1" />
                          Remove
                        </Button>
                      </div>
                      
                      <div className="bg-git-bg-secondary rounded-lg p-4">
                        <details className="group">
                          <summary className="cursor-pointer text-sm font-medium text-git-text-primary hover:text-git-accent-primary flex items-center gap-2">
                            <span>View Student Mappings</span>
                            <span className="group-open:rotate-180 transition-transform">▼</span>
                          </summary>
                          <div className="mt-4 space-y-3">
                            {config.aliases.map((alias: any, index: number) => (
                              <div key={index} className="p-3 bg-git-bg-primary rounded border border-git-stroke-secondary">
                                <p className="font-medium text-git-text-primary mb-2">{alias.officialName}</p>
                                <div className="grid grid-cols-1 md:grid-cols-2 gap-2 text-xs">
                                  <div>
                                    <span className="text-git-text-secondary">Git Usernames:</span>
                                    <div className="flex flex-wrap gap-1 mt-1">
                                      {alias.gitUsernames.map((username: string, i: number) => (
                                        <span key={i} className="px-2 py-1 bg-git-accent-secondary text-git-text-primary rounded">
                                          {username}
                                        </span>
                                      ))}
                                    </div>
                                  </div>
                                  <div>
                                    <span className="text-git-text-secondary">Emails:</span>
                                    <div className="flex flex-wrap gap-1 mt-1">
                                      {alias.emails.map((email: string, i: number) => (
                                        <span key={i} className="px-2 py-1 bg-git-bg-elevated text-git-text-secondary rounded">
                                          {email}
                                        </span>
                                      ))}
                                    </div>
                                  </div>
                                </div>
                              </div>
                            ))}
                          </div>
                        </details>
                      </div>
                    </div>
                  ))}
                </div>
              )}
            </div>

            {/* Upload/Replace Section */}
            <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg">
              <div className="mb-6">
                <div className="flex items-center gap-2 mb-2">
                  <h2 className="text-2xl font-semibold text-git-text-primary">
                    {currentConfigs.length > 0 ? 'Replace Configuration' : 'Upload Configuration'}
                  </h2>
                  <InfoButton description="Upload a config file to map multiple Git accounts to single students." />
                </div>
                {currentConfigs.length > 0 && (
                  <div className="mt-2 p-3 bg-yellow-50 border border-yellow-200 rounded-lg">
                    <p className="text-sm text-yellow-800">
                      <strong>Note:</strong> Replacing your config will immediately apply the new mappings to all repositories you analyse.
                    </p>
                  </div>
                )}
              </div>

              {/* Template Download */}
              <div className="mb-6 p-4 bg-git-bg-secondary border border-git-stroke-secondary rounded-lg">
                <div className="flex items-center justify-between">
                  <div>
                    <h3 className="text-sm font-medium text-git-text-primary mb-1">Need help with the format?</h3>
                    <p className="text-xs text-git-text-secondary">Download a sample configuration file to see the expected structure.</p>
                  </div>
                  <Button 
                    variant="outline"
                    size="sm"
                    onClick={downloadTemplate}
                    className="border-git-stroke-primary text-git-text-primary hover:bg-git-bg-elevated"
                  >
                    <Download className="h-4 w-4 mr-2" />
                    Download Template
                  </Button>
                </div>
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
                      {isLoading ? 'Processing...' : (currentConfigs.length > 0 ? 'Replace Configuration' : 'Upload Configuration')}
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
                  <div className="flex items-center gap-2">
                    <AlertCircle className="h-4 w-4 text-red-600" />
                    <p className="text-red-600 text-sm">{error}</p>
                  </div>
                </div>
              )}

              {/* Success Message */}
              {successMessage && (
                <div className="mt-4 p-3 bg-green-50 border border-green-200 rounded-lg">
                  <div className="flex items-center gap-2">
                    <CheckCircle className="h-4 w-4 text-green-600" />
                    <p className="text-green-600 text-sm">{successMessage}</p>
                  </div>
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

              {/* Confirmation Dialog */}
              {showConfirmation && (
                <div className="mt-6 p-4 bg-git-bg-secondary border border-git-stroke-primary rounded-lg">
                  <h3 className="text-lg font-semibold text-git-text-primary mb-3">
                    {currentConfigs.length > 0 ? 'Confirm Configuration Replacement' : 'Confirm Configuration'}
                  </h3>
                  <div className="mb-4">
                    <p className="text-git-text-secondary mb-2">
                      <strong>Name:</strong> {parsedAliases?.name}
                    </p>
                    <p className="text-git-text-secondary mb-2">
                      <strong>Students:</strong> {parsedAliases?.aliases?.length || 0}
                    </p>
                  </div>
                  <div className="flex gap-3">
                    <Button 
                      onClick={handleConfirmSave}
                      disabled={isSaving}
                      className="bg-git-int-primary hover:bg-git-int-primary-hover text-git-int-text"
                    >
                      {isSaving ? 'Saving...' : (currentConfigs.length > 0 ? 'Replace Configuration' : 'Save Configuration')}
                    </Button>
                    <Button 
                      variant="outline"
                      onClick={handleCancelSave}
                      disabled={isSaving}
                      className="border-git-stroke-primary text-git-text-primary hover:bg-git-bg-elevated"
                    >
                      Cancel
                    </Button>
                  </div>
                </div>
              )}

              {/* Apply Button */}
              <div className="mt-6 text-center">
                <Button 
                  size="lg"
                  className="bg-git-int-primary hover:bg-git-int-primary-hover text-git-int-text px-8"
                  disabled={!selectedFile || !!error || !parsedAliases}
                  onClick={handleApply}
                >
                  {currentConfigs.length > 0 ? 'Replace Configuration' : 'Apply Configuration'}
                </Button>
              </div>

              {/* Get Started Button */}
              <div className="mt-4 text-center">
                <Button 
                  variant="outline"
                  size="lg"
                  className="border-git-stroke-primary text-git-text-primary hover:bg-git-bg-elevated px-8"
                  onClick={() => navigate('/home')}
                >
                  <Play className="h-4 w-4 mr-2" />
                  Get Started - Analyse Repository
                </Button>
                <p className="text-sm text-git-text-secondary mt-2">
                  Ready to analyse a repository with your configuration?
                </p>
              </div>
            </div>
        </div>
      </div>
    </div>
  );
};

export default AliasConfigPage;
