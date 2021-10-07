import { Dispatch, SetStateAction, useState } from 'react';

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
  setFullScreenIndex: Dispatch<SetStateAction<number>>,
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


const App = () => {
  const [fullScreenIndex, setFullScreenIndex] = useState(-1);

  return (
    <div className="App">
      <header className="App-header">
        <h1>
          About me
        </h1>
      </header>

      <div className="experience-container">
        <h2>
          Experience
        </h2>
        {experience.map((data: experienceData, idx: number) => (
          <ExperienceBox
            data={data}
            fullScreen={fullScreenIndex==idx}
            setFullScreenIndex={setFullScreenIndex}
            idx={idx}
            key={idx}
          />
        ))}
      </div>
    </div>
  );
}

export default App;
