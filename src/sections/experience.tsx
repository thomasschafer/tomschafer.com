import React from "react";
import AnimateHeight from "react-animate-height";

import { educationData, workData, experienceType } from "../data/experience";
import { ExpandedBoxContext } from "../App";

type ExperienceBoxProps = {
  data: experienceType;
  isExpanded: boolean;
  setExpandedBoxIndex: (newIndex: number) => void;
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
    <div className={`info-box ${data.type === "education" ? "education" : ""}`}>
      <div className={`info-box-inner pointer`} onClick={toggleExpanded}>
        <div className="logo-container">
          <img alt={data.title + " Logo"} src={data.logoPath} />
        </div>
        <h2>{data.title}</h2>
        <h3>{data.subtitle}</h3>
        <AnimateHeight duration={500} height={isExpanded ? 0 : "auto"}>
          <p className="read-more-text">Read more</p>
        </AnimateHeight>
        <AnimateHeight duration={500} height={isExpanded ? "auto" : 0}>
          <div className="description-text">
            <p>{data.description[0]}</p>
            <ul>
              {data.description.slice(1).map((line, idx) => (
                <li key={idx}>{line}</li>
              ))}
            </ul>
          </div>
        </AnimateHeight>
      </div>
    </div>
  );
};

type ExperienceSectionProps = {
  experienceData: Array<experienceType>;
  indexOffset: number;
};

const ExperienceSection = ({ experienceData, indexOffset }: ExperienceSectionProps) => {
  const { expandedBoxIndex, setExpandedBoxIndex } = React.useContext(ExpandedBoxContext);

  return (
    <React.Fragment>
      {experienceData.map((data: experienceType, idx: number) => {
        idx += indexOffset;
        return (
          <ExperienceBox
            data={data}
            isExpanded={expandedBoxIndex === idx}
            setExpandedBoxIndex={setExpandedBoxIndex}
            idx={idx}
            key={idx}
          />
        );
      })}
    </React.Fragment>
  );
};

export const WorkSection = () => <ExperienceSection experienceData={workData} indexOffset={0} />;

export const EducationSection = () => (
  <ExperienceSection experienceData={educationData} indexOffset={workData.length} />
);
