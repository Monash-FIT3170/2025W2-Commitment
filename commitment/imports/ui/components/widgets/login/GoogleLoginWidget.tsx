import React, { FC } from "react";
import { cx } from "class-variance-authority";
import { Button } from "@ui/components/ui/button";
import { Meteor } from "meteor/meteor";
import { useNavigate } from "react-router-dom";
import { Chrome } from "lucide-react";

export interface GoogleLoginWidgetProps {
  className?: string;
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

  return (
    <Button 
      variant="outline" 
      onClick={handleGoogleLogin}
      className={cx("w-96 max-w-md flex items-center justify-center gap-2", props.className)}
    >
      <Chrome className="w-4 h-4" />
      Log in with Google
    </Button>
  );
};

export default GoogleLoginWidget;