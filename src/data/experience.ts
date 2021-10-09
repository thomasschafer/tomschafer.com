export type experienceType = {
  title: string;
  subtitle: string;
  description: Array<string>;
  type: string;
  logoPath: string;
};

export const experience: Array<experienceType> = [
  {
    title: "Lambert Labs",
    subtitle: "Junior Python Developer",
    description: [
      "Web development for a variety of clients with varied technology stacks, mainly using Python and JavaScript/TypeScript. Highlights:",
      "Integrated React with an existing Django site for an education client to build dynamic interfaces, building REST endpoints to communicate with the backend",
      "Designed and rebuilt menu bar from scratch for legal client, updating from a hardcoded burger menu to a flat menu bar with dropdowns that can be customised in the Wagtail CMS",
      "Worked with a design agency to implement their design update into a Django + React site",
      "Communicating with multiple stakeholders from across the client businesses to understand requirements, propose ideas and demonstrate solutions",
    ],
    type: "job",
    logoPath: "images/lambert_labs_logo.jpeg",
  },
  {
    title: "Play Sports Network",
    subtitle: "Data Analyst",
    description: [
      "Using Python and SQL to perform analysis for colleagues across the business, helping inform app and social media strategies. Highlights:",
      "Analysing comment sentiment towards brands using Google’s NLP API in Python, boosting sponsorship retention",
      "Using the Python library Keras to build models predicting the success of videos based on the thumbnail, helping inform thumbnail design",
    ],
    type: "job",
    logoPath: "images/play_sports_network_logo.png",
  },
  {
    title: "University of Bristol",
    subtitle: "BSc Mathematics",
    description: ["1st"],
    type: "education",
    logoPath: "images/university_of_bristol_logo.jpeg",
  },
];
