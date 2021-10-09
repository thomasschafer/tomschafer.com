import React, { useRef } from "react";
import ReactDOM from "react-dom";

import "./scss/index.scss";
import { ExperienceSection } from "./sections/experience";
import { HeaderSection } from "./sections/header";
import { LinksSection } from "./sections/links";
import { TechnologiesSection } from "./sections/technologies";
import { divRef, divRefCurrent, section } from "./types/types";

const App = () => {
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
    <div className="App">
      <HeaderSection executeScroll={executeScroll} sections={sections} />
      {sections.map((section) => (
        <div ref={section.ref} className="experience-container" key={section.title}>
          <h2 className="margin-bottom-10">{section.title}</h2>
          {section.component}
        </div>
      ))}
    </div>
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
