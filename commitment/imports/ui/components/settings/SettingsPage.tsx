import React, { useState } from 'react';
import NavBar from '@ui/components/landing-page/NavBar';
import { Meteor } from 'meteor/meteor';
import { useTracker } from 'meteor/react-meteor-data';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@ui/components/ui/tabs';
import { Button } from '@ui/components/ui/button';
import { Upload, Info } from 'lucide-react';

export const SettingsPage: React.FC = () => {
  const user = useTracker(() => Meteor.user());
  const isLoggedIn = !!user;
  
  // manage which tab is currently active
  const [activeTab, setActiveTab] = useState('profile');

  return (
    <div className="min-h-screen bg-git-bg-primary">
      <NavBar isLoggedIn={isLoggedIn} />
      
      <div className="max-w-6xl mx-auto px-6 py-8 pt-24">
        <h1 className="text-4xl font-bold text-git-text-primary mb-4">
          Settings
        </h1>
        <p className="text-git-text-secondary text-lg mb-8">
          This is the settings page. Do you like it?
        </p>

        <Tabs value={activeTab} onValueChange={setActiveTab} className="w-full bg-[#FEFEFA] shadow-sm justify-items-start">
          {/* The tab buttons at the top (same styling as MetricsTab */}
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

              {/* Upload Area */}
              <div className="border-2 border-dashed border-git-stroke-secondary rounded-lg p-8 text-center bg-git-bg-secondary">
                <Upload className="h-12 w-12 text-git-text-secondary mx-auto mb-4" />
                <h3 className="text-lg font-semibold text-git-text-primary mb-2">
                  Drag and drop your files here
                </h3>
                <p className="text-git-text-secondary mb-6">File format accepted: JSON</p>
                
                <Button 
                  variant="secondary" 
                  size="lg"
                  className="bg-git-bg-tertiary hover:bg-git-bg-elevated text-git-text-primary border border-git-stroke-primary"
                >
                  <Upload className="h-4 w-4" />
                  Upload files
                </Button>
              </div>

              {/* Apply Button */}
              <div className="mt-6 text-center">
                <Button 
                  size="lg"
                  className="bg-git-int-primary hover:bg-git-int-primary-hover text-git-int-text px-8"
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
