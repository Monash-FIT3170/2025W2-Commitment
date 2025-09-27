import React, { useEffect } from 'react';
import LoginWidget from '/imports/ui/components/login/LoginWidget';

export interface LoginViewProps {
  defaultTab?: "login" | "signup"
}

const LoginView = (props: LoginViewProps) => {
  const defaultTab = props.defaultTab ?? "login";

  // Clear repository history when user navigates to login/signup
  useEffect(() => {
    localStorage.removeItem('lastRepoUrl');
  }, []);

  return (
    <div className="h-[90%] flex flex-col justify-center content-center ">
      <LoginWidget defaultTab={defaultTab} />
    </div>
  )
};

export default LoginView;
