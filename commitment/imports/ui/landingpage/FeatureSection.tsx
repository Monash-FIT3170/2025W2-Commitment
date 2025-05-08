import React from 'react';
import { FeatureCarousel } from "./FeatureCarousel";


export function FeatureSection() {

    return (
        <div>

            <div className="w-full text-center mt-10">
                {/* Section for mini title */}
                <p className="text-sm text-[#F1502F] font-medium font-robotoFlex tracking-wide">
                    OUR FEATURES?
                </p>
                {/* Section for big description */}
                <h1 className="text-5xl text-gray-900 mt-2 font-robotoFlex">
                    Get The Full Picture...
                </h1>
            </div>

            {/* Carousel */}

            <FeatureCarousel/>

        </div>
    )

}