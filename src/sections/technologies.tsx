import React, { useState } from "react";
import { technologies, technologyType } from "../data/technologies";

type technologyBoxProps = {
  technology: technologyType;
  isExpanded: boolean;
  setExpandedBoxIndex: React.Dispatch<React.SetStateAction<number>>;
  idx: number;
};

const TechnologyBox = ({
  technology,
  isExpanded,
  setExpandedBoxIndex,
  idx,
}: technologyBoxProps) => {
  return (
    <div key={technology.name} className="info-box--small">
      <div className="info-box-inner--left">
        <div className="logo-container--small">
          <img alt={technology.name + " Logo"} src={technology.logoPath} />
        </div>
        <div>
          <h2>{technology.name}</h2>
          <p>{technology.description}</p>
          {/* <div className="description-text">
                      <p>{data.description[0]}</p>
                      <ul>
                      {data.description.slice(1).map((line, idx) => <li key={idx}>{line}</li>)}
                      </ul>
                  </div> */}
        </div>
      </div>
    </div>
  );
};

export const TechnologiesSection = () => {
  const [expandedBoxIndex, setExpandedBoxIndex] = useState(-1);

  return (
    <React.Fragment>
      {technologies.map((technology, idx) => (
        <TechnologyBox
          technology={technology}
          isExpanded={expandedBoxIndex == idx}
          setExpandedBoxIndex={setExpandedBoxIndex}
          idx={idx}
          key={idx}
        />
      ))}
    </React.Fragment>
  );
};
