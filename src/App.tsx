import React, { useRef, useState } from "react";
import ReactDOM from "react-dom";

import "./scss/index.scss";
import { MainMenu } from "./mainMenu";
import { AboutMe } from "./sections/aboutMe";
import { EducationSection, WorkSection } from "./sections/experience";
import { HeaderSection } from "./sections/header";
import { LinksSection } from "./sections/links";
import { TechnologiesSection } from "./sections/technologies";
import { divRef, divRefCurrent, section } from "./types/types";

const App = () => {
  const [showMainMenu, setShowMainMenu] = useState(false);

  const toggleMainMenu = (currentShowMainMenu: boolean) => {
    setShowMainMenu(!currentShowMainMenu);
    if (currentShowMainMenu) {
      document.body.classList.remove("overflow-hidden");
    } else {
      document.body.classList.add("overflow-hidden");
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
    { title: "About Me", component: <AboutMe />, ref: useRef<divRefCurrent>(null) },
    { title: "Experience", component: <WorkSection />, ref: useRef<divRefCurrent>(null) },
    { title: "Education", component: <EducationSection />, ref: useRef<divRefCurrent>(null) },
    { title: "Technologies", component: <TechnologiesSection />, ref: useRef<divRefCurrent>(null) },
    { title: "Links", component: <LinksSection />, ref: useRef<divRefCurrent>(null) },
  ];

  let prevScrollpos = window.pageYOffset;

  window.onscroll = () => {
    const currentScrollPos = window.pageYOffset;
    const navBar = document.getElementById("navbar");
    if (navBar) {
      if (prevScrollpos > currentScrollPos) {
        navBar.classList.remove("navbar-hide");
      } else {
        navBar.classList.add("navbar-hide");
      }
      prevScrollpos = currentScrollPos;
    }
  };

  return (
    <React.Fragment>
      {showMainMenu && (
        <MainMenu
          sections={sections}
          toggleMainMenu={toggleMainMenu}
          executeScroll={executeScroll}
        />
      )}
      <div className="App">
        <HeaderSection
          executeScroll={executeScroll}
          sections={sections}
          showMainMenu={showMainMenu}
          toggleMainMenu={toggleMainMenu}
        />
        {sections.map((section) => (
          <div
            ref={section.ref}
            className="experience-container max-width-container"
            key={section.title}
          >
            <h2 className="margin-bottom-10">{section.title}</h2>
            {section.component}
          </div>
        ))}
      </div>
    </React.Fragment>
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
