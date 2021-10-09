import React, { useState } from "react";
import { experience, experienceType } from "../data/experience";

type ExperienceBoxProps = {
  data: experienceType;
  isExpanded: boolean;
  setExpandedBoxIndex: React.Dispatch<React.SetStateAction<number>>;
  idx: number;
};

const ExperienceBox = ({ data, isExpanded, setExpandedBoxIndex, idx }: ExperienceBoxProps) => {
  const toggleExpanded = () => {
    if (isExpanded) {
      setExpandedBoxIndex(-1);
    } else {
      setExpandedBoxIndex(idx);
    }
  };

  return (
    <div className="info-box">
      <div className={`info-box-inner pointer ${isExpanded && " active"}`} onClick={toggleExpanded}>
        <div className="logo-container">
          <img alt={data.title + " Logo"} src={data.logoPath} />
        </div>
        <h2>{data.title}</h2>
        <h3>{data.subtitle}</h3>
        <div className="description-text">
          <p>{data.description[0]}</p>
          <ul>
            {data.description.slice(1).map((line, idx) => (
              <li key={idx}>{line}</li>
            ))}
          </ul>
        </div>
      </div>
    </div>
  );
};

export const ExperienceSection = () => {
  const [expandedBoxIndex, setExpandedBoxIndex] = useState(-1);

  return (
    <React.Fragment>
      {experience.map((data: experienceType, idx: number) => (
        <ExperienceBox
          data={data}
          isExpanded={expandedBoxIndex === idx}
          setExpandedBoxIndex={setExpandedBoxIndex}
          idx={idx}
          key={idx}
        />
      ))}
    </React.Fragment>
  );
};
