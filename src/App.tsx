import React, { useRef, useState } from "react";
import ReactDOM from "react-dom";

import "./scss/index.scss";
import { MainMenu } from "./mainMenu";
import { AboutMe } from "./sections/aboutMe";
import { EducationSection, WorkSection } from "./sections/experience";
import { HeaderSection } from "./sections/header";
import { TechnologiesSection } from "./sections/technologies";
import { divRef, divRefCurrent, section } from "./types/types";

export const ExpandedBoxContext = React.createContext({
  expandedBoxIndex: -1,
  setExpandedBoxIndex: (newIndex: number) => {},
});

const App = () => {
  const [showMainMenu, setShowMainMenu] = useState(false);
  const [mainMenuIsTransparent, setMainMenuIsTransparent] = useState(true);
  const [expandedBoxIndex, setExpandedBoxIndex] = useState(-1);

  const toggleMainMenu = (currentShowMainMenu: boolean) => {
    if (currentShowMainMenu) {
      setMainMenuIsTransparent(true);
      document.body.classList.remove("overflow-hidden");
      setTimeout(() => {
        setShowMainMenu(false);
      }, 250);
    } else {
      setShowMainMenu(true);
      document.body.classList.add("overflow-hidden");
      setTimeout(() => {
        setMainMenuIsTransparent(false);
      }, 0);
    }
  };

  const executeScroll = (ref: divRef) => () => {
    console.log("ref", ref);
    if (ref.current) {
      console.log("ref.current", ref.current);
      ref.current.scrollIntoView();
      const navBar = document.getElementById("navbar");
      if (navBar) {
        setTimeout(() => {
          navBar.classList.add("navbar-hide");
        }, 0);
      }
    }
  };

  const sections: Array<section> = [
    {
      title: "About Me",
      hideTitle: true,
      component: <AboutMe />,
      ref: useRef<divRefCurrent>(null),
    },
    { title: "Experience", component: <WorkSection />, ref: useRef<divRefCurrent>(null) },
    { title: "Education", component: <EducationSection />, ref: useRef<divRefCurrent>(null) },
    { title: "Technologies", component: <TechnologiesSection />, ref: useRef<divRefCurrent>(null) },
  ];

  return (
    <ExpandedBoxContext.Provider
      value={{
        expandedBoxIndex: expandedBoxIndex,
        setExpandedBoxIndex: (newIndex: number) => {
          setExpandedBoxIndex(newIndex);
        },
      }}
    >
      {(showMainMenu || !mainMenuIsTransparent) && (
        <MainMenu
          isTransparent={mainMenuIsTransparent}
          sections={sections}
          toggleMainMenu={toggleMainMenu}
          executeScroll={executeScroll}
        />
      )}

      <div className="App">
        <HeaderSection
          showMainMenu={showMainMenu}
          mainMenuIsTransparent={mainMenuIsTransparent}
          toggleMainMenu={toggleMainMenu}
        />
        {sections.map((section) => (
          <div
            ref={section.ref}
            className="experience-container max-width-container"
            key={section.title}
          >
            {section.hideTitle ? <></> : <h2 className="margin-bottom-10">{section.title}</h2>}
            {section.component}
          </div>
        ))}
      </div>
    </ExpandedBoxContext.Provider>
  );
};

export const runApp = () => {
  ReactDOM.render(
    <React.StrictMode>
      <App />
    </React.StrictMode>,
    document.getElementById("root")
  );
};
