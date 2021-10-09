import { divRef, section } from "./types/types";

type MainMenuProps = {
  sections: Array<section>;
  closeMenu: () => void;
  executeScroll: (ref: divRef) => () => void;
};

export const MainMenu = ({ sections, closeMenu, executeScroll }: MainMenuProps) => {
  const jumpToSection = (ref: divRef) => {
    closeMenu();
    setTimeout(() => executeScroll(ref), 0);
  };

  return (
    <div id="main-menu-modal">
      <div className="main-menu-modal-inner">
        {sections.map((section) => (
          <div key={section.title} onClick={() => jumpToSection(section.ref)}>
            <h2>{section.title}</h2>
          </div>
        ))}
        <div className="close-button" onClick={closeMenu}>
          <h2>Close</h2>
        </div>
      </div>
    </div>
  );
};
