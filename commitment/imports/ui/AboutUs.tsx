import React from 'react'; 

const AboutUs = () => {
    return (
        <div className="flex flex-col justify-center items-center mt-6">
            <Header />
            <Description /> 
            <KeyFeatureImage />
        </div>

    );
};

export default AboutUs; 

const Header = () => {
    return(
        <div className="flex flex-col justify-center items-center">
                <h4 style={{color:'#F1502F'}}  className= "font-semibold">WHAT IS COMMITMENT?</h4>
                <h1 className="text-4xl font-semibold ">Your Contribution Analyser </h1>
        </div>
    );
};

const Description = () => {
    return (
        <div className="flex flex-col justify-center items-center mt-6 max-w-xl mx-auto" >
            <p className = "text-sm text-center"> In team-based coding environments, it can be difficult to understand who did what, when, and how much. 
            Commitment solves this by analyzing Git repositories and turning raw commit data into insightful visualizations.
            </p>
        </div>
    );
};

const KeyFeatureImage = () => {
    return(
        <div className = "mt-6">
            <img 
            src = "/about-us-placeholder.png"
            alt= "About us placeholder"
            className=" rounded-xl max-w-3xl mx-auto"
            />
        </div>
    );
}
