package com.google.android.game2d.api.engine;

import com.google.android.game2d.api.AbstractSprite;

import android.app.Activity;

public abstract class GameEngine {
	protected Activity activity;
	
	public GameEngine(Activity activity) {
		this.activity = activity;
	}
	
	public abstract void initialize();
	public abstract AbstractSprite getMainCharacter(); 
}
