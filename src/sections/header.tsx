import { divRef, section } from "../types/types";

type HeaderSectionProps = {
  sections: Array<section>;
  executeScroll: (ref: divRef) => () => void;
};

export const HeaderSection = ({ sections, executeScroll }: HeaderSectionProps) => (
  <header>
    <div id="navbar-padding" />
    <div id="navbar" className="App-header">
      <ul className="max-width-container">
        {sections.map((section) => (
          <li key={section.title} onClick={executeScroll(section.ref)}>
            {section.title}
          </li>
        ))}
      </ul>
    </div>
  </header>
);
