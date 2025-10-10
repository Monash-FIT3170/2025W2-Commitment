import React, { FC } from "react";
import { cx } from "class-variance-authority";
import { Button } from "@base/button";
import { Meteor } from "meteor/meteor";
import { useNavigate } from "react-router-dom";
import { Github } from "lucide-react";

export interface GithubLoginWidgetProps {
    className?: string;
    mode?: "login" | "signup";
}

const GithubLoginWidget: FC<GithubLoginWidgetProps> = (props) => {
    const navigate = useNavigate();

    const handleGithubLogin = () => {
    Meteor.loginWithGithub({}, (err) => {
        if (err) {
            alert("GitHub login failed: " + ((err as Meteor.Error)?.reason || err.message || "Unknown error"));
        } else {
            // Successfully logged in, redirect to home
            navigate("/home");
        }
    });
    };

    const buttonText = props.mode === "signup" ? "Sign up with GitHub" : "Log in with GitHub";

    return (
        <Button
            variant="secondary"
            onClick={handleGithubLogin}
            className={cx("w-full flex items-center justify-center gap-2", props.className)}
        >
            <Github className="w-4 h-4" />
            {buttonText}
        </Button>
    );
};

export default GithubLoginWidget;
