document.addEventListener("DOMContentLoaded", function () {
	const themeDropdown = document.getElementById("theme-dropdown");
	const currentTheme = localStorage.getItem("theme") || "auto";

	document.body.setAttribute("data-theme", currentTheme);
	themeDropdown.value = currentTheme;

	themeDropdown.addEventListener("change", function () {
		const selectedTheme = themeDropdown.value;
		document.body.setAttribute("data-theme", selectedTheme);
		localStorage.setItem("theme", selectedTheme);

		if (selectedTheme === "auto") {
			applyAutoTheme();
		}
	});

	if (currentTheme === "auto") {
		applyAutoTheme();
	}

	function applyAutoTheme() {
		const darkModeMediaQuery = window.matchMedia(
			"(prefers-color-scheme: dark)",
		);
		document.body.setAttribute(
			"data-theme",
			darkModeMediaQuery.matches ? "dark" : "light",
		);
		darkModeMediaQuery.addEventListener("change", function (e) {
			document.body.setAttribute("data-theme", e.matches ? "dark" : "light");
		});
	}
});
