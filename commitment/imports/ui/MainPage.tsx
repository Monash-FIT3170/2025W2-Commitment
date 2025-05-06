import React from 'react';

const MainPage = () => (
  <div className="flex flex-col gap-6">
    <div className="flex justify-center items-center mb-4">
      <img src="/logo.png" alt="Logo" className="w-[648px] h-auto" />
    </div>

    <div className="flex justify-center items-center mb-6">
      <button

        className="w-[341px] h-[87px] bg-cover bg-center rounded-full text-white text-2xl font-bold flex items-center justify-center" 
        style={{ backgroundImage: "url('/getStartedButton.png')" }}>
         </button>

    </div>

    <div className="flex justify-center items-center">

      <img src="/Description.png" alt="Description" className="w-[800px] h-auto" />
    
    </div>
  </div>
);

export default MainPage;
