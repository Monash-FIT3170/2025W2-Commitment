import React from 'react';
import LoginWidget from '@ui/components/widgets/login/LoginWidget';

export interface LoginViewProps {
  defaultTab?: "login" | "signup"
}

const LoginView = (props: LoginViewProps) => {
  const defaultTab = props.defaultTab ?? "login";

  return (
    <div className="h-screen flex flex-col justify-center content-center bg-secondary">
      <LoginWidget defaultTab={defaultTab}></LoginWidget>
    </div>
  )
};

export default LoginView;
