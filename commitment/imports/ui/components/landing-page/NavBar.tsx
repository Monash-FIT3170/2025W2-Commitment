import React from "react";

const NavBar: React.FC = () => {
  return (
    <nav
      className="shadow sticky top-0 z-50"
      style={{ backgroundColor: "#E8E8DD" }}
    >
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-16">
          <div className="flex items-center space-x-10">
            <a href="#" className="flex items-center space-x-3">
              <img src="/logo.png" alt="Logo" className="h-10 w-10" />
              <span className="text-xl text-gray-900"></span>
            </a>
            <div className="flex space-x-6">
              <a href="#" className="text-gray-700 hover:text-blue-600">
                About
              </a>
              <a href="#" className="text-gray-700 hover:text-blue-600">
                Features
              </a>
              <a href="#" className="text-gray-700 hover:text-blue-600">
                How-To
              </a>
              <a href="#" className="text-gray-700 hover:text-blue-600">
                Our Repository
              </a>
            </div>
          </div>

          <div className="flex space-x-4">
            <a href="#" className="text-gray-700 hover:text-blue-600">
              Login
            </a>
            <a href="#" className="text-gray-700 hover:text-blue-600">
              Sign Up
            </a>
          </div>
        </div>
      </div>
    </nav>
  );
};

export default NavBar;
