import React from "react";

import { LinksSection } from "./links";

export const AboutMe = () => (
  <div className="info-box">
    <div className="info-box-inner">
      <p className="about-me-text">
        Hi, I'm Tom Schafer, a software engineer. I enjoy working with TypeScript, Go and Python,
        amongst other things.
      </p>
      <div className="about-me-background">
        <img className="about-me-image" src="images/programmer.svg" alt="Programmer" />
      </div>
      <LinksSection />
    </div>
  </div>
);
