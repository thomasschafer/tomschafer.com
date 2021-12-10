const NewTabLink = ({ link }: { link: string }) => (
  <a
    className="github-link gradient-text"
    target="_blank"
    rel="noreferrer"
    onClick={(e) => {
      e.stopPropagation();
    }}
    href={link}
  >
    {link}
  </a>
);

export const FormatLinksWithCommasAndStrings = ({ links }: { links: Array<string> }) => {
  if (links.length === 1) {
    return <NewTabLink link={links[0]} />;
  }
  return (
    <>
      {links.slice(0, -1).map((link, idx) => (
        <>
          <NewTabLink link={link} />
          {idx !== links.length - 2 && ", "}
        </>
      ))}{" "}
      and {<NewTabLink link={links[links.length - 1]} />}
    </>
  );
};
