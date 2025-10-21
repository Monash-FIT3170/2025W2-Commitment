import React from "react";
import { Outlet } from "react-router-dom";
import NavBar from "./NavBar";

export default function Layout() {
  return (
    <div className="w-full min-h-screen bg-git-bg-primary text-git-text-primary">
      <NavBar />
      <Outlet />
    </div>
  );
}
