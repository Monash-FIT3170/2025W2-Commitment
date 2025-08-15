import React, { FC } from "react";
import { cx } from "class-variance-authority";
import { Button } from "@ui/components/ui/button";
import { Meteor } from "meteor/meteor";
import { useNavigate } from "react-router-dom";
import { Github } from "lucide-react";

export interface GithubLoginWidgetProps {
    className?: string;
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

    return (
        <Button 
            variant="outline" 
            onClick={handleGithubLogin}
            className={cx("w-96 max-w-md flex items-center justify-center gap-2", props.className)}
    >
        <Github className="w-4 h-4" />
        Log in with GitHub
        </Button>
    );
};

export default GithubLoginWidget;
