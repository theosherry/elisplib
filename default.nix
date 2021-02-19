{emacsPackagesNg}:

emacsPackagesNg.melpaBuild {
		pname   = "my-libraries";
		ename   = "my-libraries";
		version = "0.10";
		recipe  = builtins.toFile "recipe" ''
			(my-libraries :fetcher github
			:repo "theosherry/elisplib"
      :files (:defaults "*.winstate"))
		'';

		src = ./.;
}
