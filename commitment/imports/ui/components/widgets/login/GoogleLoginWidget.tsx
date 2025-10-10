import React, { FC } from "react";
import { cx } from "class-variance-authority";
import { Button } from "@base/button";
import { Meteor } from "meteor/meteor";
import { useNavigate } from "react-router-dom";
import { Chrome } from "lucide-react";

export interface GoogleLoginWidgetProps {
  className?: string;
  mode?: "login" | "signup";
}

const GoogleLoginWidget: FC<GoogleLoginWidgetProps> = (props) => {
  const navigate = useNavigate();

  const handleGoogleLogin = () => {
    Meteor.loginWithGoogle({}, (err) => {
      if (err) {
        alert("Google login failed: " + ((err as Meteor.Error)?.reason || err.message || "Unknown error"));
      } else {
        // Successfully logged in, redirect to home
        navigate("/home");
      }
    });
  };

  const buttonText = props.mode === "signup" ? "Sign up with Google" : "Log in with Google";

  return (
    <Button
      variant="secondary"
      onClick={handleGoogleLogin}
      className={cx("w-full flex items-center justify-center gap-2", props.className)}
    >
      <Chrome className="w-4 h-4" />
      {buttonText}
    </Button>
  );
};

export default GoogleLoginWidget;