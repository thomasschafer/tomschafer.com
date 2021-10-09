import React, { useRef, useState } from "react";
import ReactDOM from "react-dom";

import "./scss/index.scss";
import { MainMenu } from "./mainMenu";
import { ExperienceSection } from "./sections/experience";
import { HeaderSection } from "./sections/header";
import { LinksSection } from "./sections/links";
import { TechnologiesSection } from "./sections/technologies";
import { divRef, divRefCurrent, section } from "./types/types";

const App = () => {
  const [showMainMenu, setShowMainMenu] = useState(false);

  const openMenu = () => {
    setShowMainMenu(true);
    document.body.classList.add("overflow-hidden");
  };

  const closeMenu = () => {
    setShowMainMenu(false);
    document.body.classList.remove("overflow-hidden");
  };

  const executeScroll = (ref: divRef) => () => {
    console.log(ref);
    if (ref.current) {
      ref.current.scrollIntoView();
      const navBar = document.getElementById("navbar");
      console.log(navBar);
      if (navBar) {
        setTimeout(() => {
          navBar.classList.add("navbar-hide");
        }, 0);
      }
    }
  };

  const sections: Array<section> = [
    { title: "Experience", component: <ExperienceSection />, ref: useRef<divRefCurrent>(null) },
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
        <MainMenu sections={sections} closeMenu={closeMenu} executeScroll={executeScroll} />
      )}
      <div className="App">
        <HeaderSection executeScroll={executeScroll} sections={sections} openMenu={openMenu} />
        {sections.map((section) => (
          <div ref={section.ref} className="experience-container" key={section.title}>
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
