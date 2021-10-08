import React, { useRef, useState } from 'react';
import ReactDOM from 'react-dom';

import './scss/index.scss';
import { experience } from './data/experience';


type experienceData = {
  title: string,
  subtitle: string,
  description: Array<string>,
  type: string,
  logoPath: string,
}


type ExperienceBoxProps = {
  data: experienceData,
  fullScreen: boolean,
  setFullScreenIndex: React.Dispatch<React.SetStateAction<number>>,
  idx: number,
}


const ExperienceBox = ({data, fullScreen, setFullScreenIndex, idx}: ExperienceBoxProps) => {
  const toggleFullScreen = () => {
    if (fullScreen) {
      setFullScreenIndex(-1);
    } else {
      setFullScreenIndex(idx);
    }
  }

  return (
    <div className="experience-box">
      <div className={`experience-box-inner ${fullScreen && " active"}`} onClick={toggleFullScreen}>
        <div className="logo-container">
          <img alt={data.title + " Logo"} src={data.logoPath} />
        </div>
        <h2>{data.title}</h2>
        <h3>{data.subtitle}</h3>
        <div className="description-text">
          <p>{data.description[0]}</p>
          <ul>
            {data.description.slice(1).map((line, idx) => <li key={idx}>{line}</li>)}
          </ul>
        </div>
      </div>
    </div>
  )
}

type section = {
  title: string,
  component: React.ReactElement,
  ref: any, //TODO: FIX
}

type HeaderSectionProps = {
  sections: Array<section>,
  executeScroll: (ref: any) => () => void, //TODO FIX
}

const HeaderSection = ({ sections, executeScroll }: HeaderSectionProps) => (
  <header>
    <div id="navbar-padding" />
    <div id="navbar" className="App-header">
      <ul className="max-width-container">
        {sections.map(section => (
          <li key={section.title} onClick={executeScroll(section.ref)}>{section.title}</li>
        ))}
      </ul>
    </div>
  </header>
)


const ExperienceSection = () => {
  const [fullScreenIndex, setFullScreenIndex] = useState(-1);

  return (
    <React.Fragment>
      {experience.map((data: experienceData, idx: number) => (
        <ExperienceBox
          data={data}
          fullScreen={fullScreenIndex==idx}
          setFullScreenIndex={setFullScreenIndex}
          idx={idx}
          key={idx}
        />
      ))}
    </React.Fragment>
  )
}


const App = () => {
  const executeScroll = (ref: any) => () => { // TODO: FIX ANY
    if (ref.current) {
      ref.current.scrollIntoView();
      const navBar = document.getElementById("navbar");
      console.log(navBar);
      if (navBar) {
        setTimeout(() => {navBar.classList.add('navbar-hide');}, 0);
      }
    }
  }

  const sections: Array<section> = [
    {title: 'Experience', component: <ExperienceSection/>, ref: useRef<HTMLDivElement | null>(null)},
    {title: 'Technologies', component: <div></div>, ref: useRef<HTMLDivElement | null>(null)}
  ]

  let prevScrollpos = window.pageYOffset;

  window.onscroll = () => {
    const currentScrollPos = window.pageYOffset;
    const navBar = document.getElementById("navbar");
    if (navBar) {
      if (prevScrollpos > currentScrollPos) {
        navBar.classList.remove('navbar-hide');
      } else {
        navBar.classList.add('navbar-hide');
      }
      prevScrollpos = currentScrollPos;
    }
  }

  return (
    <div className="App">
      <HeaderSection executeScroll={executeScroll} sections={sections}/>
      {sections.map(obj => ( // TODO: FIX OBJ NAME
        <div ref={obj.ref} className="experience-container" key={obj.title}>
          <h2>
            {obj.title}
          </h2>
          {obj.component}
        </div>
      ))}
    </div>
  )
}


export const runApp = () => {
  ReactDOM.render(
    <React.StrictMode>
      <App />
    </React.StrictMode>,
    document.getElementById('root')
  );
}
