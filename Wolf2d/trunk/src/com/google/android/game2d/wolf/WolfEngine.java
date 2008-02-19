package com.google.android.game2d.wolf;

import android.view.ViewGroup.LayoutParams;

import com.google.android.game2d.api.gui.BackgroundView;

public class WolfEngine {
	/* Android main activity of the game */
	private WolfActivator activator;
	/* the first scene of the game */
	private BackgroundView firstScene;
	
	public WolfEngine(WolfActivator activator) {
		this.activator = activator;
	}
	
	public void initialize() {
		ImageEngine.initialize(activator.getResources());
		firstScene = new BackgroundView(activator, ImageEngine.instance().getFirstSceneBitmap());
		firstScene.setFocusable(true);
		activator.addContentView(firstScene, new LayoutParams(LayoutParams.FILL_PARENT, LayoutParams.FILL_PARENT));
	}
	
}
