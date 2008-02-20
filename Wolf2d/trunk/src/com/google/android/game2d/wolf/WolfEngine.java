package com.google.android.game2d.wolf;

import android.app.Activity;
import android.view.ViewGroup.LayoutParams;

import com.google.android.game2d.api.AbstractSprite;
import com.google.android.game2d.api.engine.GameEngine;
import com.google.android.game2d.api.gui.BackgroundView;
import com.google.android.game2d.wolf.sprites.wolverine.WolverineSprite;

public class WolfEngine extends GameEngine {
	
	/* the first scene of the game */
	private BackgroundView firstScene;
	
	public WolfEngine(Activity activity) {
		super(activity);
	}
	
	@Override
	public void initialize() {
		firstScene = new BackgroundView(activity, WolfFactory.instance(), ImageEngine.instance().getFirstSceneBitmap());
		firstScene.setFocusable(true);
		activity.addContentView(firstScene, new LayoutParams(LayoutParams.FILL_PARENT, LayoutParams.FILL_PARENT));
	}

	@Override
	public AbstractSprite getMainCharacter() {
		return WolverineSprite.instance();
	}
	
}
