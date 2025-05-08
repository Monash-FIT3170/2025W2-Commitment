import React from 'react'

interface FeatureCardProps {
  title: string
  description: string
  image: string
  alt: string
  className?: string
}

export const FeatureCard = ({ title, description, image, alt, className = '' }: FeatureCardProps) => {
  return (
    <div className={`border-2 border-orange-500 bg-white rounded-xl shadow p-4 text-center ${className}`}>
      <h2 className="text-xl font-semibold text-gray-800 mb-4">{title}</h2>
      <img
        src={image}
        alt={alt}
        className="w-full h-48 object-cover rounded-md mb-4"
      />
      <p className="text-gray-600 text-sm">{description}</p>
    </div>
  )
}