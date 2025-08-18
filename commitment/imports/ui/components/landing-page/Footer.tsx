import React from "react";

function Logo() {
  return (
    <div className="flex items-start justify-start">
      <img src="/logo.svg" alt="Logo" className="w-[50px] h-auto" />
    </div>
  );
}

function Commitment() {
  return (
    <div>
      <h3 className="font-bold font-mono mb-3">Commitment</h3>
      <ul>
        <li>
          <span className="font-mono cursor-pointer hover:underline">
            How it Works
          </span>
        </li>
        <li>
          <span className="font-mono cursor-pointer hover:underline">Docs</span>
        </li>
      </ul>
    </div>
  );
}

function Support() {
  return (
    <div>
      <h3 className="font-mono font-bold mb-3">Support</h3>
      <ul>
        <li>
          <span className="font-mono cursor-pointer hover:underline">
            Resources
          </span>
        </li>
        <li>
          <span className="font-mono cursor-pointer hover:underline">
            GitHub Repository
          </span>
        </li>
      </ul>
    </div>
  );
}

function Legal() {
  return (
    <div>
      <h3 className="font-mono font-bold mb-3">Legal</h3>
      <ul>
        <li>
          <span className="font-mono cursor-pointer hover:underline">
            License
          </span>
        </li>
      </ul>
    </div>
  );
}

function Footer() {
  return (
    <footer className="bg-[#E8E8DD] text-black p-10 rounded-lg shadow-lg ml-32 mr-32 mb-6">
      <div className="max-w-7xl mx-auto grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-8">
        <div>
          <Logo />
        </div>
        <div>
          <Commitment />
        </div>
        <div>
          <Support />
        </div>
        <div>
          <Legal />
        </div>
      </div>

      <div className="text-center pt-8 border-t border-gray-700 mt- font-mono">
        <p>
          &copy; {new Date().getFullYear()} Copyright Commitment. All Rights
          Reserved
        </p>
      </div>
    </footer>
  );
}

export default Footer;
