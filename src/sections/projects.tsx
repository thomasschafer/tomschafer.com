import React from "react";
import AnimateHeight from "react-animate-height";

import { projectData, projectType } from "../data/projects";
import { ExpandedBoxContext } from "../App";
import { FormatLinksWithCommasAndStrings } from "../utils";

const HEIGHT_ANIMATION_DURATION_MS = 250;

type ExperienceBoxProps = {
  data: projectType;
  isExpanded: boolean;
  setExpandedBoxId: (newId: string) => void;
};

const ProjectBox = ({ data, isExpanded, setExpandedBoxId }: ExperienceBoxProps) => {
  const toggleExpanded = () => {
    if (isExpanded) {
      setExpandedBoxId("");
    } else {
      setExpandedBoxId(data.projectId);
    }
  };

  return (
    <div className={"info-box hover-effect"}>
      <div className={"info-box-inner pointer"} onClick={toggleExpanded}>
        <div className="image-container">
          <img alt={data.title + " Logo"} src={data.imagePath} />
        </div>
        <a
          className="project-link"
          href={data.link}
          target="_blank"
          rel="noreferrer"
          onClick={(e) => {
            e.stopPropagation();
          }}
        >
          <h2 className="gradient-text gradient-underline">{data.title}</h2>
        </a>
        <h3>{data.subtitle}</h3>
        <AnimateHeight duration={HEIGHT_ANIMATION_DURATION_MS} height={isExpanded ? 0 : "auto"}>
          <p className="read-more-text">Read more</p>
        </AnimateHeight>
        <AnimateHeight
          className="description-text-container"
          duration={HEIGHT_ANIMATION_DURATION_MS}
          height={isExpanded ? "auto" : 0}
        >
          <div className="description-text">
            <ul>
              {data.description.map((line, idx) => (
                <li key={idx}>{line}</li>
              ))}
            </ul>
            Code available at{" "}
            {data.githubLinks && <FormatLinksWithCommasAndStrings links={data.githubLinks} />}
          </div>
        </AnimateHeight>
      </div>
    </div>
  );
};

export const ProjectsSection = () => {
  const { expandedBoxId, setExpandedBoxId } = React.useContext(ExpandedBoxContext);

  return (
    <React.Fragment>
      {projectData.map((data: projectType, idx: number) => {
        return (
          <ProjectBox
            data={data}
            isExpanded={expandedBoxId === data.projectId}
            setExpandedBoxId={setExpandedBoxId}
            key={idx}
          />
        );
      })}
    </React.Fragment>
  );
};
