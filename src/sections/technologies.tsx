import React from "react";
import { technologies, technologyType } from "../data/technologies";

type technologyBoxProps = {
  technology: technologyType;
  idx: number;
};

const TechnologyBox = ({ technology, idx }: technologyBoxProps) => {
  return (
    <div key={technology.name} className={`info-box--small ${idx === 0 ? "margin-top-20" : ""}`}>
      <div className="info-box-inner--left">
        <div className="logo-container--small">
          <img alt={technology.name + " Logo"} src={technology.logoPath} />
        </div>
        <div>
          <h2>{technology.name}</h2>
          <p>{technology.description}</p>
        </div>
      </div>
    </div>
  );
};

export const TechnologiesSection = () => (
  <React.Fragment>
    {technologies.map((technology, idx) => (
      <TechnologyBox technology={technology} idx={idx} key={idx} />
    ))}
  </React.Fragment>
);
