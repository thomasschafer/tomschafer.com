export type experienceType = {
  title: string;
  subtitle: string;
  description: Array<string>;
  type: string;
  logoPath: string;
};

export const workData: Array<experienceType> = [
  {
    title: "Lambert Labs",
    subtitle: "Software Engineer",
    description: [
      "Development for a variety of clients with varied technology stacks, mainly using Python and JavaScript/TypeScript. Highlights:",
      "Integrated React with an existing Django site for an education client to build dynamic messaging dashboard",
      "Built REST endpoints in Django to communicate with React frontends",
      "Rebuilt various parts a legal client's website, using a combination of Django templating, vanilla Javascript and React with Typescript",
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
];

export const educationData: Array<experienceType> = [
  {
    title: "University of Bristol",
    subtitle: "BSc Mathematics",
    description: [
      "First class honours (final grade 76%), including 89% in final year project on p-adic numbers.",
    ],
    type: "education",
    logoPath: "images/university_of_bristol_logo.jpeg",
  },
];
