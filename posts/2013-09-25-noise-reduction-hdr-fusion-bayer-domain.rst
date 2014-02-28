---
title:  Noise reduction in images through HDR fusion in the Bayer domain
tags: hdr, high dynamic range, hdr fusion, high dynamic range fusion, noise, noise reduction, noise measurement, image processing, raw, camera, photography, tonemapping, signal noise ratio, snr
---

Introduction
------------

This is a brief post about my final year project in computer
engineering. I've developed a small command line application to do high
dynamic range fusion on RAW images (Bayer domain). I've coded it in C++
using `Halide <http://halide-lang.org/>`__ and
`LibRaw <http://www.libraw.org/>`__.

This little application takes some RAW files, 2 or more, as input and
returns a HDR TIFF image.

RAWs I've used to get the images showed in this post belongs to `The HDR Book: Unlocking the Pros Hottest Post-Processing Techniques <http://kelbytraining.com/product/the-hdr-book-unlocking-the-pros-hottest-post-processing-techniques-2/>`__ (I haven't read it).


Noise measurement
-----------------

If we take a photography of an *uniform zone* with an ideal camera all
pixels in the image will have the same value. But the perfect camera
does not exist. This image will always have some interference, noise,
that will break the uniformity making pixels fluctuate around a value,
the mean value. So, we can equate average value with noiseless value and
standard deviation with noise. Hence, we can express signal to noise
ratio (SNR) this way:

.. math:: SNR(image) = \frac{\mu(image)}{\sigma(image)}


We can meassure the SNR for different relative exposures by taking some
pictures to an uniform zone (using the manual focus to blur the image in
order to avoid textures) with different exposure times.

.. figure:: ../images/2013-09-25-noise-reduction-hdr-fusion-bayer-domain/snr.png

HDR fusion
----------

Reading the previous chart we realize we need to increase exposure,
`exposing to the
right <http://en.wikipedia.org/wiki/Exposing_to_the_right>`__ if we want
to get less noise. But there is a limit, the saturation point. This is
the point at which it can no longer generate new electrons for
additional photon strikes.

We could take two photos, one well-exposed and another one overexposed.
The well-exposed picture will have detail in the highlights but more
noise than the overexposed picture. The overexposed image will have the
opposite features, much less noise and saturated highlights. Equalising
the exposure level and taking the best parts of each image (the
highlight from the well-exposed picture, the non-saturated parts form
the overexposed one) we can get a image with higher dynamic range and
less noise. This is called High Dynamic Range fusion.

Fusion Map
----------

The key to get a fused HDR image with lower noise is in the generation
of the fusion map. In the case of two input images, this approach
binarizes the more exposed picture with the next formula to get a fusion
map where pixels with value 0 are fitted with the well-exposed picture,
and pixel with value 1, with the over-exposed one:


.. math:: map(x,y) = overexposed(x,y) \leq (saturation \cdot threshold)


The above formula has a threshold value between 0 and 1 to take more
(higher threshold) or less (lower value) highlights from de overexposed
image. The binarized image is blurred with Guassian kernel to avoid sudden
transitions in the final image.

Extending this process to support more than two images is pretty simple.

.. raw:: html

	<div class="flex-row flex-images">

		<div>
			<img src="../images/2013-09-25-noise-reduction-hdr-fusion-bayer-domain/test7-0EV.jpeg">
		</div>
		<div>
			<img src="../images/2013-09-25-noise-reduction-hdr-fusion-bayer-domain/test7-2EV.jpeg">
		</div>
		<div>
			<img src="../images/2013-09-25-noise-reduction-hdr-fusion-bayer-domain/test7-4EV.jpeg">
		</div>
	</div>

	<div class="flex-row flex-images">
		<div>
			<img src="../images/2013-09-25-noise-reduction-hdr-fusion-bayer-domain/map7.jpeg">
		</div>
		<div>
			<img src="../images/2013-09-25-noise-reduction-hdr-fusion-bayer-domain/test7-tm.jpeg">
		</div>
	 </div>


Bayer Domain
------------

There is a diference between my application and common approaches, it
uses directly RAW images. A RAW image contains only one channel per
pixel (R,G or B). So, while other aplications use raster images, which
have one real and two interpolated channels, this application only works
with real data. This way, the performance may be improved.


Noise reduction
---------------

The level of noise reduction depends on the exposure schema. A scheme
like [0 EV, +3 EV, +6 EV] works pretty well. A wider exposure scheme can
increase the noise reduction, but it could generate artifacts (visible
fusion borders) due to the difference SNR between pixels from photos
with a high exposure level difference.


.. raw:: html

	<div class="flex-row flex-images">
		<div>
			<img 	src="../images/2013-09-25-noise-reduction-hdr-fusion-bayer-domain/0EV-detail.jpeg">
		</div>
		<div>
			<img src="../images/2013-09-25-noise-reduction-hdr-fusion-bayer-domain/hdr-detail.jpeg">
	 	</div>
	 </div>

Tonemapping
-----------

This application uses the less exposed photography as reference, so the
fusion result has the same look. This way we can avoid the
`tonemapping <http://en.wikipedia.org/wiki/Tone_mapping>`__ process.
Nevertheless, by applying a tonemapping operator we can get a better
result if the less exposed image is *too* underexposed. Anyway, this
application does not include a tonemapping option, but the user could use
an external tool like
`pfstmo <http://pfstools.sourceforge.net/pfstmo.html>`__.


Next steps
----------

I may open source this application in the future. But, firstly, I want to change some parts an fix some bugs. If someone want me to explain this
project more deeply or something about digital photography or image processing, just ask it.


Thanks
------

I would like to thank some people for their support. In the first place,
I want to express my gratitude to my girlfriend,
`Ángela <https://twitter.com/angelagesteiras>`__, for all those hours in
the library and for her photographies, advices and help. Next, I want to mention `Guillermo
Luijk <http://www.guillermoluijk.com/>`__, his work on digital
photography is the base of this project. I also want to thank
`Quobis <https://twitter.com/quobis>`__, the company I work for (they
have been unselfish and gave me the posibility to shift my working
schedule), and my friends `Aitor <https://twitter.com/ATuin>`__ and
`Eleazar <https://twitter.com/EleDiaz777>`__ for encouraging me.
