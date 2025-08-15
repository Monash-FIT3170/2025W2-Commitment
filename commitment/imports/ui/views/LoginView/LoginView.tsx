import React from "react";
import LoginWidget from "@ui/components/widgets/login/LoginWidget";
import GoogleLoginWidget from "@ui/components/widgets/login/GoogleLoginWidget";
import GithubLoginWidget from "@ui/components/widgets/login/GithubLoginWidget";

const LoginView = () => (
  <div className="h-screen flex flex-col justify-center items-center bg-secondary gap-6">
    <LoginWidget />
    <GoogleLoginWidget />
    <GithubLoginWidget />
  </div>
);

export default LoginView;