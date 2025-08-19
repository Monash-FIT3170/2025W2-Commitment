import React from 'react';
import LoginWidget from '@ui/components/widgets/login/LoginWidget';

function LoginView() {
  return (
    <div className="h-screen flex flex-col justify-center content-center bg-secondary">
      <LoginWidget />
    </div>
  );
}

export default LoginView;
